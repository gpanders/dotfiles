(fn setlocal [opt ?val]
  (let [opt (tostring opt)]
    (if ?val
      `(set ,(sym (.. "vim.opt_local." opt)) ,?val)
      (match (opt:gsub "&$" "")
        (where (o n) (> n 0))
          `(let [default# (. (vim.api.nvim_get_option_info ,o) :default)]
             (set ,(sym (.. "vim.opt_local." o)) default#))
        _ (match (opt:gsub "^no" "")
            (o n) `(set ,(sym (.. "vim.opt_local." o)) ,(= n 0)))))))

(fn setlocal+= [opt val]
  `(,(: "vim.opt_local.%s:append" :format (tostring opt)) ,val))

(fn setlocal^= [opt val]
  `(,(: "vim.opt_local.%s:prepend" :format (tostring opt)) ,val))

(fn setlocal-= [opt val]
  `(,(: "vim.opt_local.%s:remove" :format (tostring opt)) ,val))

(fn exec [s]
  `(vim.api.nvim_command ,s))

(fn make-ident [...]
  "Create a unique identifier for a global function"
  (-> (icollect [_ v (pairs [...])]
        (when (= (type v) :string) (v:lower)))
      (table.concat "_")
      (string.gsub "-" "_")
      (string.gsub "[^%w_]+" "")
      (.. (tostring (gensym)))))

(fn keymap [mode from to ?opts]
  "Map a key in the given mode. Defaults to non-recursive, use {:noremap false}
in opts to use a recursive mapping.

Examples:

  (keymap :n \"Y\" \"y$\")
  (keymap :n \"j\" \"(v:count == 0 ? 'gj' : 'j')\" {:expr true})"
  (assert (= (type mode) :string) "mode should be a string")
  (assert (= (type from) :string) "from should be a string")
  (assert (or (= nil ?opts) (= (type ?opts) :table)) "opts should be a table")
  (let [opts (or ?opts {})
        form `(do)
        to (match (type to)
             :string to
             _ (let [ns (make-ident :keymap (if opts.buffer :buf nil) mode from)]
                 (table.insert form `(tset _G ,ns ,to))
                 (if opts.expr
                     (: "v:lua.%s()" :format ns)
                     (: "<Cmd>call v:lua.%s()<CR>" :format ns))))]
    (when (= opts.noremap nil)
      (set opts.noremap true))
    (if opts.buffer
        (let [buf (match opts.buffer
                    true 0
                    n n)]
          (set opts.buffer nil)
          (table.insert form `(vim.api.nvim_buf_set_keymap ,buf ,mode ,from ,to ,opts)))
        (table.insert form `(vim.api.nvim_set_keymap ,mode ,from ,to ,opts)))
    form))

(fn autocmd [group event pat flags ...]
  (assert (= (type group) :string) "autocmd group should be a string")
  (assert (or (= (type event) :string) (sequence? event)) "autocmd event should be a string or list")
  (assert (or (= (type flags) :string) (sequence? flags)) "autocmd flags should be a string or list")
  (let [events (if (sequence? event) (table.concat event ",") event)
        ns (make-ident :au group (events:gsub "," "_"))
        flags (icollect [_ v (ipairs (if (sequence? flags) flags [flags]))]
                (: "++%s" :format v))]
    `(do
      (global ,(sym ns) (fn [] ,...))
      (exec ,(.. "augroup " group))
      ,(let [cmd (: "autocmd! %s %%s %s call v:lua.%s()" :format events (table.concat flags " ") ns)]
        `(exec
          ,(if (list? pat)
               `(: ,cmd :format ,pat)
               (cmd:format pat))))
      (exec "augroup END"))))

(fn augroup [group ...]
  (let [form `(do)]
    (each [_ au (pairs [...])]
      (table.insert au 2 group)
      (table.insert form au))
    form))

(fn command [cmd opts func]
  (let [ns (make-ident :comm cmd)
        attrs (icollect [k v (pairs opts)]
                (: "-%s=%s" :format k v))]
    `(do
      (global ,(sym ns) ,func)
      (exec ,(: "command! %s %s call v:lua.%s(<bang>0, <q-mods>, <q-args>)" :format (table.concat attrs " ") cmd ns)))))

(fn append! [str s]
  "Append to a string in place"
  `(set ,str (.. (or ,str "") ,s)))

(fn with-module [module-binding ...]
  (let [[binding name] module-binding]
  `(match (pcall require ,name)
    (true ,binding) (do ,...))))

(fn empty-or-nil? [s]
  `(or (= ,s nil) (= (length ,s) 0)))

{
  : setlocal
  : setlocal+=
  : setlocal^=
  : setlocal-=
  : exec
  : keymap
  : autocmd
  : augroup
  : command
  : append!
  : with-module
  : empty-or-nil?
}
