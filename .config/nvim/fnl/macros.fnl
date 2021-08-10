(fn setlocal [opt ?val]
  (let [opt (string.format "%s" opt)]
    (if ?val
      `(set ,(sym (.. "vim.opt_local." opt)) ,?val)
      (match (string.gsub opt "&$" "")
        (where (o n) (> n 0))
          `(let [default# (. (vim.api.nvim_get_option_info ,o) :default)]
             (set ,(sym (.. "vim.opt_local." o)) default#))
        _ (match (string.gsub opt "^no" "")
            (o n) `(set ,(sym (.. "vim.opt_local." o)) ,(= n 0)))))))

(fn setlocal+= [opt val]
  `(,(string.format "vim.opt_local.%s:append" opt) ,val))

(fn setlocal^= [opt val]
  `(,(string.format "vim.opt_local.%s:prepend" opt) ,val))

(fn setlocal-= [opt val]
  `(,(string.format "vim.opt_local.%s:remove" opt) ,val))

(fn exec [s]
  `(vim.api.nvim_command ,s))

(fn noremap [mode from to ?opts]
  (let [opts (or ?opts {})
        mode (string.format "%s" mode)]
    (tset opts :noremap true)
    (if opts.buffer
        (do
          (tset opts :buffer nil)
          `(vim.api.nvim_buf_set_keymap ,mode ,from ,to ,opts))
        `(vim.api.nvim_set_keymap ,mode ,from ,to ,opts))))

(fn autocmd [group event pat ...]
  ; Use timestamp as a unique identifier for a global function
  (local ns (string.format "fnl%d" (os.time)))
  `(do
    (tset _G ,ns (fn [] ,...))
    (vim.api.nvim_command ,(.. "augroup " group))
    (vim.api.nvim_command ,(string.format "autocmd! %s %s call v:lua.%s(expand('<amatch>:p'))" event pat ns))
    (vim.api.nvim_command "augroup END")))

(fn append! [str s]
  `(set ,str (.. (or ,str "") ,s)))

{
  : setlocal
  : setlocal+=
  : setlocal^=
  : setlocal-=
  : exec
  : noremap
  : autocmd
  : append!
}
