; Do not refresh scrollbar in insert mode
(let [view (require :satellite.view)
      orig view.refresh_bars]
  (fn view.refresh_bars [...]
    (let [{: mode} (nvim.get_mode)]
      (when (not= :i (mode:sub 1 1))
        (orig ...)))))

(let [satellite (require :satellite)]
  (satellite.setup {:current_only true
                    :handlers {:marks {:enable false}
                               :cursor {:enable false}
                               :gitsigns {:enable false}
                               :quickfix {:enable false}
                               :diagnostic {:min_severity vim.diagnostic.severity.INFO}}}))

(set vim.o.wrapmargin 1)
