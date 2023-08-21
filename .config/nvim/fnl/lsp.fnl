(when (= nil vim.g.lsp)
  (set vim.g.lsp {:autostart true}))

(fn on-init [client result]
  (let [lsp-compl (require :lsp_compl)]
    (set vim.lsp.text_document_completion_list_to_complete_items lsp-compl.text_document_completion_list_to_complete_items)
    (when (client.supports_method :textDocument/signatureHelp)
      (set client.server_capabilities.signatureHelpProvider.triggerCharacters [])))
  (match result.offsetEncoding
    enc (set client.offset_encoding enc)))

(fn hover [_ result ctx]
  ((. vim.lsp.handlers "textDocument/hover") _ result ctx {:border :rounded}))

(fn signature-help [_ result ctx]
  (vim.lsp.handlers.signature_help _ result ctx {:focusable false
                                                 :border :rounded}))

(local handlers {"textDocument/hover" hover
                 "textDocument/signatureHelp" signature-help})

(local capabilities (vim.tbl_deep_extend :force
                                         (vim.lsp.protocol.make_client_capabilities)
                                         {:general {:positionEncodings [:utf-8 :utf-16]}
                                          :workspace {:didChangeWatchedFiles {:dynamicRegistration false}}}
                                         ((. (require :lsp_compl) :capabilities))))

(fn start [config]
  (when (and (= 1 (vim.fn.executable (. config.cmd 1)))
             (not= vim.g.lsp.autostart false)
             (= vim.bo.buftype "")
             (vim.uv.fs_access (nvim.buf_get_name 0) :r)
             (nvim.buf_is_loaded 0))
    (let [root-dir (if config.root
                       (let [[root-marker] (vim.fs.find config.root {:upward true})]
                         (vim.fs.dirname root-marker)))]
      (vim.lsp.start (vim.tbl_extend :keep config {:root_dir (or root-dir (vim.uv.cwd))
                                                   :on_init on-init
                                                   : capabilities
                                                   : handlers})))))

(fn enable []
  (set vim.g.lsp.autostart true)
  (let [curbuf (nvim.get_current_buf)]
    (start curbuf)
    (each [_ buf (ipairs (nvim.list_bufs))]
      (when (and (not= curbuf buf) (nvim.buf_is_loaded buf))
        (autocmd :BufEnter {:buffer buf :once true} #(start buf))))))

(fn disable []
  (set vim.g.lsp.autostart false)
  (vim.lsp.stop_client (vim.lsp.get_clients)))

{: start
 : enable
 : disable}
