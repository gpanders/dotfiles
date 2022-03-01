(fn make-colors [colors ...]
  (assert-compile (= 0 (math.fmod (select :# ...) 2))
                  "expected even number of group/option pairs")
  (let [form `(do)
        highlights []]
    (for [i 1 (select :# ...) 2]
      (let [(group opts) (select i ...)
            group (tostring group)]
        (table.insert form `(vim.api.nvim_set_hl 0
                                                 ,group
                                                 ,(if opts.link
                                                      `{:link ,opts.link}
                                                      `{:ctermfg ,(or (?. colors opts.fg :cterm) `nil)
                                                        :ctermbg ,(or (?. colors opts.bg :cterm) `nil)
                                                        :fg ,(or (?. colors opts.fg :gui) `nil)
                                                        :bg ,(or (?. colors opts.bg :gui) `nil)
                                                        :sp ,(or (?. colors opts.guisp :gui) `nil)
                                                        :bold ,(if (= opts.attr :bold) 1 0)
                                                        :italic ,(if (= opts.attr :italic) 1 0)
                                                        :underline ,(if (= opts.attr :underline) 1 0)
                                                        :undercurl ,(if (= opts.attr :undercurl) 1 0)})))))
   form))

{: make-colors}
