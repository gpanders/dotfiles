(var enabled true)
(local configs {})

(let [orig vim.uri_from_bufnr]
  (fn vim.uri_from_bufnr [bufnr]
    (let [fname (vim.api.nvim_buf_get_name bufnr)]
      (if (fname:find "^fugitive://")
          (vim.uri_from_fname (vim.call :FugitiveReal fname))
          (orig bufnr)))))

(fn dirname [path]
  (vim.fn.fnamemodify path ":h"))

(fn find-root [start patterns]
  (let [pattern-map (collect [_ v (ipairs patterns)] (values v true))
        test (partial . pattern-map)]
    (var done? false)
    (var curdir start)
    (while (not done?)
      (match (length (vim.fn.readdir curdir test))
        0 (let [parent (dirname curdir)]
            (if (= parent curdir)
                (do
                  (set curdir nil)
                  (set done? true))
                (set curdir parent)))
        _ (set done? true)))
    curdir))

(fn on-attach [client bufnr]
  (when client.resolved_capabilities.completion
    (tset vim.bo bufnr :omnifunc "v:lua.vim.lsp.omnifunc"))
  (when client.resolved_capabilities.goto_definition
    (tset vim.bo bufnr :tagfunc "v:lua.vim.lsp.tagfunc"))
  (when client.resolved_capabilities.document_highlight
    (with-module [ts-config :nvim-treesitter.configs]
      (ts-config.detach_module :refactor.highlight_definitions bufnr))
    (augroup lsp
      (autocmd :CursorHold "<buffer>" (vim.lsp.buf.document_highlight))
      (autocmd [:InsertEnter :CursorMoved] "<buffer>" (vim.lsp.buf.clear_references))))
  (keymap :i "<C-S>" #(vim.lsp.buf.signature_help) {:buffer bufnr})
  (keymap :n "[R" #(vim.lsp.buf.references) {:buffer bufnr})
  (keymap :n "crr" #(vim.lsp.buf.rename) {:buffer bufnr})
  (keymap :n "<Bslash>K" #(vim.lsp.buf.hover) {:buffer bufnr})
  (keymap :n "cac" #(vim.lsp.buf.code_action) {:buffer bufnr})

  (with-module [lsp-compl :lsp_compl]
    (vim.opt.completeopt:append [:noinsert])
    (lsp-compl.attach client bufnr {}))

  (tset vim.b bufnr :lsp {:name client.name :client_id client.id})

  (exec "doautocmd User LspAttached"))

(fn on-init [client result]
  (with-module [lsp-compl :lsp_compl]
    (set vim.lsp.text_document_completion_list_to_complete_items lsp-compl.text_document_completion_list_to_complete_items)
    (set client.resolved_capabilities.signature_help_trigger_characters []))
  (when result.offsetEncoding
    (set client.offset_encoding result.offsetEncoding))
  (when client.config.settings
    (client.notify :workspace/didChangeConfiguration {:settings client.config.settings})))

(fn on-exit [code signal client-id]
  (each [_ bufnr (ipairs (vim.lsp.get_buffers_by_client_id client-id))]
    (tset vim.b bufnr :lsp nil)
    (vim.schedule #(do
                     (setlocal tagfunc nil)
                     (setlocal omnifunc nil)
                     (exec (: "autocmd! lsp * <buffer=%d>" :format bufnr))))))

(fn mk-config [cmd ?root-dir ?opts]
  (let [capabilities (vim.lsp.protocol.make_client_capabilities)]
    (set capabilities.workspace.configuration true)
    (vim.tbl_deep_extend :keep (or ?opts {})
      {:flags {:debounce_text_changes 80
               :allow_incremental_sync true}
       : cmd
       :name (. cmd 1)
       :handlers {}
       : capabilities
       :on_init on-init
       :on_attach on-attach
       :on_exit on-exit
       :root_dir ?root-dir})))

(fn start-client [bufnr {: cmd : root &as opts}]
  (when (= (vim.fn.executable (. cmd 1)) 1)
    (let [root (icollect [_ v (ipairs [".git" ".hg" ".svn"]) :into root]
                 v)
          root-dir (let [dir (dirname (vim.api.nvim_buf_get_name bufnr))]
                     (find-root dir root))
          ft (. vim.bo bufnr :filetype)
          clients (. configs ft :clients)]
      (var client-id (. clients root-dir))
      (when (not client-id)
        (let [config (mk-config cmd root-dir opts)]
          (set client-id (vim.lsp.start_client config))
          (when root-dir
            (tset clients root-dir client-id))))
      (vim.lsp.buf_attach_client bufnr client-id))))

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
             :offset_encoding :utf-16}
  [:go :gomod] {:cmd [:gopls]
                :root ["go.mod"]
                :settings {:gopls {:analyses {:unusedparams true
                                              :unusedwrite true
                                              :nilness true}}}}
  :lua {:cmd [:lua-language-server]
        :root [".luarc.json"]
        :settings {:Lua {:telemetry {:enable false}}}}
  :zig {:cmd [:zls]
        :root ["build.zig" "zls.json"]}
  :python {:cmd ["pyright-langserver" "--stdio"]
           :root ["pyproject.toml" "setup.py" "setup.cfg" "requirements.txt" "Pipfile" "pyrightconfig.json"]}
  :rust {:cmd [:rust-analyzer]
         :root ["Cargo.toml"]})

(fn lsp-start [bufnr]
  (let [ft (. vim.bo bufnr :filetype)]
    (match (. configs ft)
      opts (start-client bufnr opts))))

(autocmd lsp :FileType "*"
  (when enabled
    (let [bufnr (-> "<abuf>" vim.fn.expand tonumber)]
      (lsp-start bufnr))))

(let [commands {:stop #(each [client-id (pairs (vim.lsp.buf_get_clients))]
                         (vim.lsp.stop_client client-id))
                :detach #(each [client-id (pairs (vim.lsp.buf_get_clients))]
                           (on-exit nil nil client-id)
                           (vim.lsp.buf_detach_client 0 client-id))
                :disable #(do
                            (set enabled false)
                            (each [client-id (pairs (vim.lsp.get_active_clients))]
                              (vim.lsp.stop_client client-id)))
                :enable #(do
                           (set enabled true)
                           (each [_ bufnr (ipairs (vim.api.nvim_list_bufs))]
                             (lsp-start bufnr)))
                :start #(let [bufnr (vim.api.nvim_get_current_buf)]
                          (lsp-start bufnr))
                :find #(match $1
                         nil (vim.lsp.buf.definition)
                         q (vim.lsp.buf.workspace_symbol q))
                :code_action #(vim.lsp.buf.code_action)
                :hover #(vim.lsp.buf.hover)
                :format #(vim.lsp.buf.formatting)
                :references #(vim.lsp.buf.references)
                :rename #(vim.lsp.buf.rename $1)
                :signature_help #(vim.lsp.buf.signature_help)}
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
                       0 (vim.api.nvim_err_writeln (: "Invalid command: %s" :format cmd))
                       _ (vim.api.nvim_err_writeln (: "Ambiguous command: %s can match any of %s" :format cmd (table.concat matches ", ")))))))))

  (exec "cnoreabbrev <expr> lsp (getcmdtype() ==# ':' && getcmdline() ==# 'lsp') ? 'Lsp' : 'lsp'"))
