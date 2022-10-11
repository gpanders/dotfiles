(when (= nil vim.g.lsp_autostart)
  (set vim.g.lsp_autostart false))

(local configs {})

(var set-log-level false)

(augroup lsp#
  (autocmd :LspAttach
    (fn [{: buf :data {: client_id}}]
      (local client (vim.lsp.get_client_by_id client_id))
      (tset vim.b buf :lsp_client client.name)
      (when client.server_capabilities.documentHighlightProvider
        (augroup lsp#
          (autocmd [:CursorHold :InsertLeave] {:buffer buf} vim.lsp.buf.document_highlight)
          (autocmd [:CursorMoved :InsertEnter] {:buffer buf} vim.lsp.buf.clear_references)))
      (when client.server_capabilities.hoverProvider
        (keymap :n "K" vim.lsp.buf.hover {:buffer buf}))
      (keymap :n "[R" vim.lsp.buf.references {:buffer buf})
      (keymap :i "<C-S>" vim.lsp.buf.signature_help {:buffer buf})
      (keymap :n "<Space>cr" vim.lsp.buf.rename {:buffer buf})
      (keymap :n "<Space>ca" vim.lsp.buf.code_action {:buffer buf})

      (let [lsp-compl (require :lsp_compl)]
        (match client.name
          :lua-language-server (set client.server_capabilities.completionProvider.triggerCharacters ["." ":"]))
        (vim.cmd "set completeopt+=noinsert")
        (lsp-compl.attach client buf {}))))
  (autocmd :LspDetach
    (fn [{: buf :data {: client_id}}]
      (tset vim.b buf :lsp_client nil)
      (let [lsp-compl (require :lsp_compl)]
        (lsp-compl.detach client_id buf))
      (autocmd! lsp# "*" {:buffer buf}))))

(fn on-init [client result]
  (let [lsp-compl (require :lsp_compl)]
    (set vim.lsp.text_document_completion_list_to_complete_items lsp-compl.text_document_completion_list_to_complete_items)
    (when client.server_capabilities.signatureHelpProvider
      (set client.server_capabilities.signatureHelpProvider.triggerCharacters [])))
  (when result.offsetEncoding
    (set client.offset_encoding result.offsetEncoding)))

(fn hover [_ result ctx]
  ((. vim.lsp.handlers "textDocument/hover") _ result ctx {:border :rounded}))

(fn signature-help [_ result ctx]
  (vim.lsp.handlers.signature_help _ result ctx {:focusable false
                                                 :border :rounded}))

(local handlers {"textDocument/hover" hover
                 "textDocument/signatureHelp" signature-help})

(fn lsp-start [bufnr]
  (let [ft (. vim.bo bufnr :filetype)]
    (match (. configs ft)
      config (when (= 1 (vim.fn.executable (. config.cmd 1)))
               (let [[root-marker] (vim.fs.find config.root {:upward true})
                     root-dir (vim.fs.dirname root-marker)]
                 (when (not set-log-level)
                   (vim.lsp.set_log_level :OFF)
                   (set set-log-level true))
                 (vim.lsp.start (vim.tbl_extend :keep config {:root_dir root-dir
                                                              :on_init on-init
                                                              : handlers})))))))

(macro lsp-setup [...]
 (assert-compile (= 0 (math.fmod (select :# ...) 2))
                 "expected even number of filetype/config pairs")
 (let [form `(do)]
   (for [i 1 (select :# ...) 2]
     (let [(filetypes config) (select i ...)
           (first rest) (match filetypes
                          [first & rest] (values first rest)
                          _ (values filetypes []))]
       (table.insert form `(tset configs ,first ,config))
       (each [_ ft (ipairs rest)]
         (table.insert form `(tset configs ,ft (. configs ,first))))))
   form))

(lsp-setup
  [:c :cpp] {:cmd ["clangd" "--background-index"]
             :root [".clangd" ".clang-format" "compile_commands.json" "compile_flags.txt"]
             :flags {:debounce_text_changes 20}
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
           :name "pyright"
           :root ["pyproject.toml" "setup.py" "setup.cfg" "requirements.txt" "Pipfile" "pyrightconfig.json"]}
  :rust {:cmd [:rust-analyzer]
         :root ["Cargo.toml"]}
  :haskell {:cmd ["haskell-language-server-wrapper" "--lsp"]
            :name :hls
            :root ["*.cabal" "stack.yaml" "cabal.project" "package.yaml" "hie.yaml"]
            :settings {:haskell {:formattingProvider :ormolu}}})

(autocmd lsp# :FileType "*"
  (fn [{: buf}]
    (when (and (not= vim.g.lsp_autostart false)
               (nvim.buf_is_valid buf)
               (nvim.buf_is_loaded buf))
      (lsp-start buf))))

(let [commands {:stop #(each [client-id (pairs (vim.lsp.get_active_clients))]
                         (vim.lsp.stop_client client-id))
                :detach #(let [buf (nvim.get_current_buf)]
                           (each [_ client (ipairs (vim.lsp.get_active_clients {:bufnr buf}))]
                             (vim.lsp.buf_detach_client buf client.id)))
                :disable #(do
                            (set vim.g.lsp_autostart false)
                            (vim.lsp.stop_client (vim.lsp.get_active_clients)))
                :enable #(do
                           (set vim.g.lsp_autostart true)
                           (let [curbuf (nvim.get_current_buf)]
                             (lsp-start curbuf)
                             (each [_ buf (ipairs (nvim.list_bufs))]
                               (when (and (not= curbuf buf) (nvim.buf_is_loaded buf))
                                 (autocmd :BufEnter {:buffer buf :once true} #(lsp-start buf))))))
                :start #(lsp-start (nvim.get_current_buf))
                :log #(match $1
                        nil (echo (: "LSP log level is %s" :format (. vim.lsp.log_levels ((. (require "vim.lsp.log") :get_level)))))
                        level (do
                                (vim.lsp.set_log_level (level:upper))
                                (set set-log-level true)
                                (echo (: "LSP log level set to %s" :format level))))}
      complete (fn [arg line pos]
                 (icollect [cmd (pairs commands)]
                   (if (= arg (string.sub cmd 1 (length arg)))
                       cmd)))]
  (command :Lsp {:nargs "*" : complete}
           (fn [{: fargs}]
             (let [[cmd & args] fargs]
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
  (keymap :n "<Space>cc" #(if vim.g.lsp_autostart
                              (commands.disable)
                              (commands.enable))))
