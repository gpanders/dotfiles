(fn setlocal [opt ?val]
  (let [opt (tostring opt)]
    (if ?val
      `(tset vim.opt_local ,opt ,?val)
      (match (opt:gsub "&$" "")
        (where (o n) (> n 0)) `(let [{:default default#} (vim.api.nvim_get_option_info ,o)]
                                 (tset vim.opt_local ,o default#))
        _ (match (opt:gsub "^no" "")
            (o n) `(tset vim.opt_local ,o ,(= n 0)))))))

(fn setlocal+= [opt val]
  `(: (. vim.opt_local ,(tostring opt)) :append ,val))

(fn setlocal^= [opt val]
  `(: (. vim.opt_local ,(tostring opt)) :prepend ,val))

(fn setlocal-= [opt val]
  `(: (. vim.opt_local ,(tostring opt)) :remove ,val))

(fn exec [s]
  `(vim.api.nvim_command ,s))

(fn make-ident [key ...]
  "Create a unique identifier for a global function"
  (-> (icollect [_ v (pairs [...])]
        (when (= (type v) :string) (v:lower)))
      (table.concat "_")
      (string.gsub "-" "_")
      (string.gsub "[^%w_]+" "")
      (->> (.. (tostring (gensym key))))))

(fn keymap [modes from to ?opts]
  "Map a key in the given mode. Defaults to non-recursive and silent.

Examples:

  (keymap :n \"Y\" \"y$\")
  (keymap :n \"j\" \"(v:count == 0 ? 'gj' : 'j')\" {:expr true})"
  (assert (or (= (type modes) :string) (sequence? modes)) "modes should be a list or string")
  (assert (= (type from) :string) "from should be a string")
  (assert (or (= nil ?opts) (= (type ?opts) :table)) "opts should be a table")
  (let [form `(do)
        opts (or ?opts {})
        modes (if (sequence? modes) modes [modes])
        to (match (type to)
             :string to
             _ (let [ns (make-ident :keymap (if opts.buffer :buf nil) from)]
                 (table.insert form `(tset _G ,ns ,to))
                 (if opts.expr
                     (: "v:lua.%s()" :format ns)
                     (: "<Cmd>call v:lua.%s()<CR>" :format ns))))]
    (when (= opts.noremap nil)
      (set opts.noremap true))
    (when (= opts.silent nil)
      (set opts.silent true))
    (let [buf (match opts.buffer
                true 0
                n n)]
      (set opts.buffer nil)
      (each [_ mode (pairs modes)]
        (if buf
            (table.insert form `(vim.api.nvim_buf_set_keymap ,buf ,mode ,from ,to ,opts))
            (table.insert form `(vim.api.nvim_set_keymap ,mode ,from ,to ,opts)))))
    form))

(fn autocmd [...]
  (let [args [...]
        group (if (sym? (. args 1)) (tostring (table.remove args 1)))
        [events pat & args] args
        events (if (sequence? events) (table.concat events ",") events)
        flags (icollect [_ v (ipairs args) :until (not= (type v) :string)]
                (do
                  (table.remove args 1)
                  (: "++%s" :format v)))
        ns (make-ident :au (or group :default) (events:gsub "," "_"))
        form `(do
                (global ,(sym ns) (fn [] ,(unpack args))))]
    (when group
      (table.insert form `(exec ,(.. "augroup " group))))
    (let [cmd (: "autocmd %s %%s %s call v:lua.%s()" :format events (table.concat flags " ") ns)]
      (table.insert form `(exec ,(if (list? pat)
                                     `(: ,cmd :format ,pat)
                                     (cmd:format pat)))))
    (when group
      (table.insert form `(exec "augroup END")))
    form))

(fn augroup [group ...]
  (let [form `(do)]
    (each [_ au (pairs [...])]
      (when (= (tostring (. au 1)) :autocmd)
        (table.insert au 2 group))
      (table.insert form au))
    form))

(fn command [cmd opts func]
  (let [ns (make-ident :comm cmd)
        attrs (icollect [k v (pairs opts)]
                (if (= (type v) :boolean)
                    (if v (.. "-" k) nil)
                    (: "-%s=%s" :format k v)))]
    `(do
      (global ,(sym ns) ,func)
      (exec ,(: "command! %s %s call v:lua.%s(<bang>0, <q-mods>, <q-args>)" :format (table.concat attrs " ") cmd ns)))))

(fn append! [str s]
  "Append to a string in place"
  `(set ,str (.. (or ,str "") ,s)))

(fn with-module [module-binding ...]
  "Binds a module to the given name and executes the forms.

Example:

  (with-module [mod :mymodule]
    (mod.func)
    (mod.bar))

The example above is equivalent to

  (match (pcall require :mymodule)
    (true mod) (do
                 (mod.func)
                 (mod.bar)))"
  (let [[binding name] module-binding]
    `(match (pcall require ,name)
       (true ,binding) (do ,...))))

(fn empty-or-nil? [s]
  `(or (= ,s nil) (= (length ,s) 0)))

(fn printf [s ...]
  `(print (: ,s :format ,...)))

(fn tinsert [t key val]
  "Like tset, but uses table.insert to add val to the table at t[key].

Example:

  (local t {})
  (tinsert t :l \"foo\") => t = { l = {'foo'} }
"
  `(do
     (when (not (. ,t ,key))
      (tset ,t ,key []))
    (table.insert (. ,t ,key) ,val)))

{: setlocal
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
 : printf
 : tinsert}
