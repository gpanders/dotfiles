(fn on-exit [_ code]
  (when (> code 0)
    (echo (: "ctags failed with code %d" :format code) :WarningMsg)))

(fn generate-tags [args]
  (let [cmd (: "ctags -R %s" :format args)]
    (vim.fn.jobstart cmd {:on_exit on-exit})))

(command :Tags {:nargs :*} #(generate-tags $3))
