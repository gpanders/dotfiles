(when (= nil vim.g.lsp)
  (set vim.g.lsp {:autostart true}))

(local compl (match (pcall require :lsp_compl)
               (true compl) compl
               _ nil))

(fn on-init [client result]
  (when compl
    (set vim.lsp.text_document_completion_list_to_complete_items compl.text_document_completion_list_to_complete_items)
    (when (client.supports_method :textDocument/signatureHelp)
      (set client.server_capabilities.signatureHelpProvider.triggerCharacters [])))
  (match result.offsetEncoding
    enc (set client.offset_encoding enc)))

(fn hover [_ result ctx config]
  (let [opts {:border :rounded}
        config (collect [k v (pairs (or config {})) &into opts]
                 (values k v))]
    (vim.lsp.handlers.hover _ result ctx config)))

(fn signature-help [_ result ctx config]
  (let [opts {:focusable false
              :border :rounded}
        config (collect [k v (pairs (or config {})) &into opts]
                 (values k v))]
    (vim.lsp.handlers.signature_help _ result ctx config)))

(local handlers {"textDocument/hover" hover
                 "textDocument/signatureHelp" signature-help})

(local capabilities (vim.tbl_deep_extend :force
                                         (vim.lsp.protocol.make_client_capabilities)
                                         {:general {:positionEncodings [:utf-8 :utf-16]}}
                                         (if compl (compl.capabilities) {})))

(fn start [config]
  (when (and (= 1 (vim.fn.executable (. config.cmd 1)))
             (not= vim.g.lsp.autostart false)
             (= vim.bo.buftype "")
             (vim.uv.fs_access (nvim.buf_get_name 0) :r)
             (nvim.buf_is_loaded 0))
    (let [root-dir (if config.root
                       (let [[root-marker] (vim.fs.find config.root {:upward true})]
                         (vim.fs.dirname root-marker)))]
      (vim.lsp.start (vim.tbl_extend :keep config {:name (. config.cmd 1)
                                                   :root_dir (or root-dir (vim.uv.cwd))
                                                   :on_init on-init
                                                   : capabilities
                                                   : handlers})))))

(fn enable []
  (set vim.g.lsp.autostart true)
  (exec "doautoall <nomodeline> FileType"))

(fn disable []
  (set vim.g.lsp.autostart false)
  (vim.lsp.stop_client (vim.lsp.get_clients)))

{: start
 : enable
 : disable}
