(when (= nil vim.g.lsp_autostart)
  (set vim.g.lsp_autostart false))

(local configs {})
(local state {})

(fn dirname [path]
  (vim.fn.fnamemodify path ":h"))

(fn once [f]
  (var called false)
  (fn [...]
    (when (not called)
      (f ...)
      (set called true))))

(local set-initial-log-level (once vim.lsp.set_log_level))

(fn find-root [start patterns]
  (var done? false)
  (var curdir start)
  (while (not done?)
    (each [_ pattern (ipairs patterns) :until done?]
      (set done? (not= "" (vim.fn.globpath curdir pattern))))
    (when (not done?)
      (let [parent (dirname curdir)]
        (if (= parent curdir)
            (do
              (set curdir nil)
              (set done? true))
            (set curdir parent)))))
  curdir)

(fn on-attach [client bufnr]
  (tset state bufnr {})
  (when client.server_capabilities.completionProvider
    (tset vim.bo bufnr :omnifunc "v:lua.vim.lsp.omnifunc"))
  (when client.server_capabilities.definitionProvider
    (tset vim.bo bufnr :tagfunc "v:lua.vim.lsp.tagfunc"))
  (when client.server_capabilities.documentHighlightProvider
    (when (not (. state bufnr :timer))
      (tset state bufnr :timer (vim.loop.new_timer)))
    (let [timer (. state bufnr :timer)]
      (augroup lsp#
        (autocmd :CursorMoved {:buffer bufnr}
          #(let [[row col] (nvim.win.get_cursor 0)
                 lnum (- row 1)
                 references (. state bufnr :references)]
             (timer:stop)
             (var found? false)
             (each [_ {:range {: start : end}} (ipairs (or references [])) :until found?]
               (when (and (= start.line end.line lnum)
                          (<= start.character col end.character))
                 (set found? true)))
             (when (not found?)
               (vim.lsp.util.buf_clear_references bufnr)
               (timer:start 150 0 #(vim.schedule vim.lsp.buf.document_highlight)))))
        (autocmd [:InsertEnter :BufLeave] {:buffer bufnr}
          (fn []
            (timer:stop)
            (vim.lsp.util.buf_clear_references bufnr))))))
  (when client.server_capabilities.hoverProvider
    (keymap :n "K" vim.lsp.buf.hover {:buffer bufnr}))
  (keymap :n "[R" vim.lsp.buf.references {:buffer bufnr})
  (keymap :i "<C-S>" vim.lsp.buf.signature_help {:buffer bufnr})
  (keymap :n "cR" vim.lsp.buf.rename {:buffer bufnr})
  (keymap :n "cac" vim.lsp.buf.code_action {:buffer bufnr})

  (with-module [lsp-compl :lsp_compl]
    (vim.cmd "set completeopt+=noinsert")
    (lsp-compl.attach client bufnr {}))

  (nvim.exec_autocmds :User {:pattern :LspAttached}))

(fn on-detach [bufnr]
  (match (. state bufnr :timer)
    timer (timer:close))
  (tset state bufnr nil)
  (vim.schedule #(do
                   (nvim.set_option_value :tagfunc nil {:scope :local})
                   (nvim.set_option_value :omnifunc nil {:scope :local})
                   (autocmd! lsp# "*" {:buffer bufnr}))))

(fn on-init [client result]
  (with-module [lsp-compl :lsp_compl]
    (set vim.lsp.text_document_completion_list_to_complete_items lsp-compl.text_document_completion_list_to_complete_items)
    (when client.server_capabilities.signatureHelpProvider
      (set client.server_capabilities.signatureHelpProvider.triggerCharacters [])))
  (when result.offsetEncoding
    (set client.offset_encoding result.offsetEncoding))
  (when client.config.settings
    (client.notify :workspace/didChangeConfiguration {:settings client.config.settings})))

(fn on-exit [code signal client-id]
  (each [_ bufnr (ipairs (vim.lsp.get_buffers_by_client_id client-id))]
    (on-detach bufnr)))

(fn hover [_ result ctx]
  ((. vim.lsp.handlers "textDocument/hover") _ result ctx {:border :rounded
                                                           :focusable false}))
(fn signature-help [_ result ctx]
  (vim.lsp.handlers.signature_help _ result ctx {:focusable false}))

(fn document-highlight [_ result ctx]
  (let [references (or result [])]
    (tset state ctx.bufnr :references references)
    (vim.lsp.util.buf_clear_references bufnr)
    ((. vim.lsp.handlers "textDocument/documentHighlight") _ references ctx)))

(local handlers {"textDocument/hover" hover
                 "textDocument/signatureHelp" signature-help
                 "textDocument/documentHighlight" document-highlight})

(fn mk-config [cmd ?root-dir ?opts]
  (let [capabilities (vim.lsp.protocol.make_client_capabilities)]
    (set capabilities.workspace.configuration true)
    (vim.tbl_deep_extend :keep (or ?opts {})
      {:flags {:allow_incremental_sync true}
       : cmd
       :name (. cmd 1)
       : handlers
       : capabilities
       :on_init on-init
       :on_attach on-attach
       :on_exit on-exit
       :root_dir ?root-dir})))

(fn start-client [bufnr ft {: cmd : root &as opts}]
  (when (= (vim.fn.executable (. cmd 1)) 1)
    (let [root-markers (icollect [_ v (ipairs [".git" ".hg" ".svn"]) :into root] v)
          root-dir (let [dir (dirname (nvim.buf.get_name bufnr))]
                     (find-root dir root-markers))
          clients (. configs ft :clients)]
      (set-initial-log-level :OFF)
      (var client-id (. clients root-dir))
      (when (or (not client-id) (vim.lsp.client_is_stopped client-id))
        (let [config (mk-config cmd root-dir opts)]
          (set client-id (vim.lsp.start_client config))
          (when root-dir
            (tset clients root-dir client-id))))
      (if (not (vim.lsp.buf_attach_client bufnr client-id))
          (echo (: "LSP client %s failed to attach to buffer %d" :format (. cmd 1) bufnr) :WarningMsg))))
  nil)

(macro lsp-setup [...]
  (assert-compile (= 0 (math.fmod (select :# ...) 2))
                  "expected even number of filetype/config pairs")
  (let [form `(do)]
    (for [i 1 (select :# ...) 2]
      (let [(filetypes opts) (select i ...)
            opts (collect [k v (pairs opts) :into {:clients {}}]
                   (values k v))
            (first rest) (match filetypes
                           [first & rest] (values first rest)
                           _ (values filetypes []))]
        (table.insert form `(tset configs ,first ,opts))
        (each [_ ft (ipairs rest)]
          (table.insert form `(tset configs ,ft (. configs ,first))))))
    form))

(lsp-setup
  [:c :cpp] {:cmd ["clangd" "--background-index"]
             :root ["compile_commands.json" "compile_flags.txt"]
             :flags {:debounce_text_changes 20}
             :offset_encoding :utf-16}
  [:go :gomod] {:cmd [:gopls]
                :root ["go.mod"]
                :settings {:gopls {:analyses {:unusedparams true
                                              :unusedwrite true
                                              :nilness true}}}}
  :lua {:cmd [:lua-language-server]
        :root [".luarc.json"]
        :settings {:Lua {:telemetry {:enable false}}}
        :on_attach (fn [client bufnr]
                     (set client.server_capabilities.completionProvider.triggerCharacters ["." ":"])
                     (on-attach client bufnr))}
  :zig {:cmd [:zls]
        :root ["build.zig" "zls.json"]}
  :python {:cmd ["pyright-langserver" "--stdio"]
           :name "pyright"
           :root ["pyproject.toml" "setup.py" "setup.cfg" "requirements.txt" "Pipfile" "pyrightconfig.json"]}
  :rust {:cmd [:rust-analyzer]
         :root ["Cargo.toml"]})

(fn lsp-start [bufnr]
  (let [ft (. vim.bo bufnr :filetype)]
    (match (. configs ft)
      opts (start-client bufnr ft opts))))

(autocmd lsp# :FileType "*"
  (fn [{: buf}]
    (when (and (not= vim.g.lsp_autostart false) (nvim.buf.is_valid buf) (nvim.buf.is_loaded buf))
      (lsp-start buf))))

(let [commands {:stop #(each [client-id (pairs (vim.lsp.get_active_clients))]
                         (vim.lsp.stop_client client-id))
                :detach #(let [buf nvim.current.buf]
                           (each [client-id (pairs (vim.lsp.buf_get_clients buf.id))]
                             (vim.lsp.buf_detach_client buf.id client-id)
                             (on-detach buf.id)))
                :disable #(do
                            (set vim.g.lsp_autostart false)
                            (each [client-id (pairs (vim.lsp.get_active_clients))]
                              (vim.lsp.stop_client client-id)))
                :enable #(let [buf nvim.current.buf]
                           (set vim.g.lsp_autostart true)
                           (lsp-start buf.id))
                :start #(let [buf nvim.current.buf]
                          (lsp-start buf.id))
                :find #(match $1
                         nil (vim.lsp.buf.definition)
                         q (vim.lsp.buf.workspace_symbol q))
                :code_action #(vim.lsp.buf.code_action)
                :hover #(vim.lsp.buf.hover)
                :format #(vim.lsp.buf.formatting)
                :references #(vim.lsp.buf.references)
                :rename #(vim.lsp.buf.rename $1)
                :signature_help #(vim.lsp.buf.signature_help)
                :log #(match $1
                        nil (echo (: "LSP log level is %s" :format (. vim.lsp.log_levels ((. (require "vim.lsp.log") :get_level)))))
                        level (do
                                (vim.lsp.set_log_level (level:upper))
                                (echo (: "LSP log level set to %s" :format level))))}
      complete (fn [arg line pos]
                 (icollect [cmd (pairs commands)]
                   (if (= arg (string.sub cmd 1 (length arg)))
                       cmd)))]
  (command :Lsp {:nargs "*" : complete}
           (fn [{: args}]
             (let [[cmd & args] (vim.split args " ")]
               (match (. commands cmd)
                 f (f (unpack args))
                 _ (let [matches (icollect [k (pairs commands)]
                                   (when (= cmd (string.sub k 1 (length cmd)))
                                     k))]
                     (match (length matches)
                       1 ((. commands (. matches 1)) (unpack args))
                       0 (nvim.err_writeln (: "Invalid command: %s" :format cmd))
                       _ (nvim.err_writeln (: "Ambiguous command: %s can match any of %s" :format cmd (table.concat matches ", ")))))))))

  (vim.cmd "cnoreabbrev <expr> lsp (getcmdtype() ==# ':' && getcmdline() ==# 'lsp') ? 'Lsp' : 'lsp'")
  (keymap :n "yo<Space>" #(if vim.g.lsp_autostart
                              (commands.disable)
                              (commands.enable))))
