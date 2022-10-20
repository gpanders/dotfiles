(local configs {})

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

(fn start [bufnr]
  (let [ft (. vim.bo bufnr :filetype)]
    (match (. configs ft)
      config (when (= 1 (vim.fn.executable (. config.cmd 1)))
               (let [[root-marker] (vim.fs.find config.root {:upward true})
                     root-dir (vim.fs.dirname root-marker)]
                 (vim.lsp.start (vim.tbl_extend :keep config {:root_dir root-dir
                                                              :on_init on-init
                                                              : handlers})))))))

(fn enable []
  (set vim.g.lsp_autostart true)
  (let [curbuf (nvim.get_current_buf)]
    (start curbuf)
    (each [_ buf (ipairs (nvim.list_bufs))]
      (when (and (not= curbuf buf) (nvim.buf_is_loaded buf))
        (autocmd :BufEnter {:buffer buf :once true} #(start buf))))))

(fn disable []
  (set vim.g.lsp_autostart false)
  (vim.lsp.stop_client (vim.lsp.get_active_clients)))

(macro setup [...]
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

(setup
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

{: start
 : enable
 : disable}
