; Do not refresh scrollbar in insert mode
(let [view (require :satellite.view)
      orig view.refresh_bars]
  (fn view.refresh_bars [...]
    (let [{: mode} (nvim.get_mode)]
      (when (not= :i (mode:sub 1 1))
        (orig ...)))))

(let [satellite (require :satellite)]
  (satellite.setup {:handlers {:marks {:enable false}}}))
