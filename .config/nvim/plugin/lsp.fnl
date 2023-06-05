(when (= nil vim.g.lsp)
  (set vim.g.lsp {:autostart true}))

(augroup lsp#
  (autocmd :LspAttach
    (fn [{: buf :data {: client_id}}]
      (local client (vim.lsp.get_client_by_id client_id))
      (tset vim.b buf :lsp_client client.name)
      (when (client.supports_method :textDocument/documentHighlight)
        (augroup lsp#
          (autocmd [:CursorHold :InsertLeave] {:buffer buf} vim.lsp.buf.document_highlight)
          (autocmd [:CursorMoved :InsertEnter] {:buffer buf} vim.lsp.buf.clear_references)))
      (when (client.supports_method :textDocument/hover)
        (keymap :n "<Space>k" vim.lsp.buf.hover {:buffer buf}))
      (keymap :n "[R" vim.lsp.buf.references {:buffer buf})
      (keymap :i "<C-S>" vim.lsp.buf.signature_help {:buffer buf})
      (keymap :n "<Space>r" vim.lsp.buf.rename {:buffer buf})
      (keymap :n "<Space>a" #(vim.lsp.buf.code_action {:apply true}) {:buffer buf})

      (autocmd lsp# :BufWritePre {:buffer buf}
        #(when (vim.F.if_nil (?. vim.b.lsp :autoformat) vim.g.lsp.autoformat false)
           (vim.lsp.buf.format {:bufnr buf})))

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

(autocmd lsp# :FileType "*"
  (fn [{: buf}]
    (when (and (not= vim.g.lsp.autostart false)
               (= (. vim.bo buf :buftype) "")
               (vim.uv.fs_access (nvim.buf_get_name buf) :r)
               (nvim.buf_is_valid buf)
               (nvim.buf_is_loaded buf))
      (let [lsp (require :lsp)]
        (lsp.start buf)))))

(let [commands {:stop #(each [client-id (pairs (vim.lsp.get_active_clients))]
                         (vim.lsp.stop_client client-id))
                :detach #(let [buf (nvim.get_current_buf)]
                           (each [_ client (ipairs (vim.lsp.get_active_clients {:bufnr buf}))]
                             (vim.lsp.buf_detach_client buf client.id)))
                :disable #((. (require :lsp) :disable))
                :enable #((. (require :lsp) :enable))
                :start #((. (require :lsp) :start) (nvim.get_current_buf))
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
  (keymap :n "<Space>cc" #(let [lsp (require :lsp)]
                            (if vim.g.lsp.autostart
                                (lsp.disable)
                                (lsp.enable)))))
