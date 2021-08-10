(fn setlocal [opt ?val]
  (let [opt (tostring opt)]
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

(fn keymap [mode from to ?opts]
  "Map a key in the given mode. Defaults to non-recursive, use {:noremap false}
in opts to use a recursive mapping.

Examples:

  (keymap :n \"Y\" \"y$\")
  (keymap :n \"j\" \"(v:count == 0 ? 'gj' : 'j')\" {:expr true})"
  (assert (= (type mode) :string) "mode should be a string")
  (assert (= (type from) :string) "from should be a string")
  (assert (= (type to) :string) "to should be a string")
  (assert (or (= `nil ?opts) (table? ?opts)) "opts should be a table")
  (let [opts (or ?opts {})]
    (tset opts :noremap true)
    (if opts.buffer
        (let [buf (match opts.buffer
                    true 0
                    n n)]
          (tset opts :buffer nil)
          `(vim.api.nvim_buf_set_keymap ,buf ,mode ,from ,to ,opts))
        `(vim.api.nvim_set_keymap ,mode ,from ,to ,opts))))

; Create a unique identifier for a global function
(fn make-ident [...]
  (-> (icollect [_ v (ipairs [...])]
        (when (= (type v) :string) (v:lower)))
      (table.concat "_")
      (string.gsub "-" "_")
      (->> (.. "fnl" (tostring (gensym))))))

(fn autocmd [group event pat flags ...]
  (assert (= (type group) :string) "autocmd group should be a string")
  (assert (or (= (type event) :string) (sequence? event)) "autocmd event should be a string or list")
  (assert (or (= (type flags) :string) (sequence? flags)) "autocmd flags should be a string or list")
  (let [events (if (sequence? event) (table.concat event ",") event)
        ns (make-ident :au group (string.gsub events "," "-"))
        flags (table.concat (icollect [_ v (ipairs (if (sequence? flags) flags [flags]))]
                              (string.format "++%s" v)) " ")]
    `(do
      (tset _G ,ns (fn [] ,...))
      (exec ,(.. "augroup " group))
      ,(let [cmd (string.format "autocmd! %s %%s %s call v:lua.%s()" events flags ns)]
        `(exec
          ,(if (list? pat)
               `(string.format ,cmd ,pat)
               (string.format cmd pat))))
      (exec "augroup END"))))

(fn command! [cmd opts func]
  (let [ns (make-ident :comm cmd)
        attrs (icollect [k v (pairs opts)]
                (string.format "-%s=%s" k v))]
    `(do
      (tset _G ,ns ,func)
      (exec ,(string.format "command! %s %s call v:lua.%s(<bang>0, <q-mods>, <q-args>)" (table.concat attrs " ") cmd ns)))))

(fn append! [str s]
  `(set ,str (.. (or ,str "") ,s)))

{
  : setlocal
  : setlocal+=
  : setlocal^=
  : setlocal-=
  : exec
  : keymap
  : autocmd
  : command!
  : append!
}
