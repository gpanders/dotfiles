(fn make-tag-item [name range uri]
  (let [{: line : character} range.start]
    {: name
     :filename (vim.uri_to_fname uri)
     :cmd (: "call cursor(%d, %d)" :format (+ line 1) (+ character 1))}))

(fn query-definition [pattern]
  (let [params (vim.lsp.util.make_position_params)
        (results-per-client err) (vim.lsp.buf_request_sync 0 "textDocument/definition" params 1000)
        results []]
    (assert (not err) (vim.inspect err))
    (fn add [range uri]
      (table.insert results (make-tag-item pattern range uri)))
    (each [_ v (pairs results-per-client)]
      (let [result (or v.result {})]
        (if result.range
            (add result.range result.uri)
            (each [_ item (pairs result)]
              (if item.range
                  (add item.range item.uri)
                  (add item.targetSelectionRange item.targetUri))))))
    results))

(fn query-workspace-symbols [pattern]
  (let [(results-per-client err) (vim.lsp.buf_request_sync 0 "workspace/symbol" {:query pattern} 1000)]
    (assert (not err) (vim.inspect err))
    (icollect [_ symbols (pairs results-per-client)]
      (each [_ symbol (pairs (or symbols.result {}))]
        (let [loc symbol.location
              item (make-tag-item symbol.name loc.range loc.uri)]
          (set item.kind (or (. vim.lsp.protocol.SymbolKind symbol.kind) :Unknown))
          item)))))

(autocmd lsp :FileType "go,c,cpp,rust,python,zig" :once
  (exec "packadd nvim-lspconfig")
  (with-module [lspconfig :lspconfig]
    (fn on-attach [client bufnr]
      (tset vim.bo bufnr :omnifunc "v:lua.vim.lsp.omnifunc")
      (tset vim.bo bufnr :tagfunc "v:lua.vim.lsp.tagfunc")

      (keymap :n "gr" "<Cmd>lua vim.lsp.buf.references()<CR>" {:buffer bufnr})
      (keymap :n "gR" "<Cmd>lua vim.lsp.buf.rename()<CR>" {:buffer bufnr})
      (keymap :n "<Bslash>c" "<Cmd>lua vim.lsp.buf.code_action()<CR>" {:buffer bufnr})

      (with-module [ts-config :nvim-treesitter.configs]
        (ts-config.detach_module :refactor.highlight_definitions bufnr))

      (when (?. client :capabilities :textDocument :documentHighlight)
        (augroup lsp
          (autocmd :CursorHold "<buffer>" (vim.lsp.buf.document_highlight))
          (autocmd [:InsertEnter :CursorMoved] "<buffer>" (vim.lsp.buf.clear_references))))

      (with-module [lsp-compl :lsp_compl]
        (vim.opt.completeopt:append [:noinsert])
        (lsp-compl.attach client bufnr {:server_side_fuzzy_completion true
                                        :trigger_on_delete true}))

      (when (not vim.lsp.tagfunc)
        (fn vim.lsp.tagfunc [pattern flags info]
          (let [tags (match flags
                       :c (query-definition pattern)
                       (where (or "" :i)) (query-workspace-symbols pattern))]
            (if (empty-or-nil? tags)
                vim.NIL
                tags))))

      (exec "doautocmd User LspAttached"))

    (let [servers {:rust_analyzer {}
                   :clangd {}
                   :gopls {:analyses {:unusedparams true :unusedwrite true :nilness true}}
                   :pyright {}
                   :zls {}}]
      (each [name settings (pairs servers)]
        (let [{name config} lspconfig]
          (config.setup {:on_attach on-attach
                         :settings {name settings}
                         :flags {:debounce_text_changes 150}})

          (when (vim.tbl_contains config.filetypes vim.bo.filetype)
            (config.manager.try_add)))))))
