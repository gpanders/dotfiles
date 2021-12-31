(fn setlocal [opt ?val]
  (assert-compile (sym? opt) "opt should be a plain symbol" opt)
  (let [opt (tostring opt)]
    (if ?val
      `(tset vim.opt_local ,opt ,?val)
      (match (opt:gsub "&$" "")
        (where (o n) (> n 0)) `(let [{:default default#} (vim.api.nvim_get_option_info ,o)]
                                 (tset vim.opt_local ,o default#))
        _ (match (opt:gsub "^no" "")
            (o n) `(tset vim.opt_local ,o ,(= n 0)))))))

(fn setlocal+= [opt val]
  (assert-compile (sym? opt) "opt should be a plain symbol" opt)
  `(: (. vim.opt_local ,(tostring opt)) :append ,val))

(fn setlocal^= [opt val]
  (assert-compile (sym? opt) "opt should be a plain symbol" opt)
  `(: (. vim.opt_local ,(tostring opt)) :prepend ,val))

(fn setlocal-= [opt val]
  (assert-compile (sym? opt) "opt should be a plain symbol" opt)
  `(: (. vim.opt_local ,(tostring opt)) :remove ,val))

(fn exec [s]
  `(vim.api.nvim_command ,s))

(fn echo [msg ?hl ?history]
  (let [history (not (not ?history))]
    `(vim.api.nvim_echo [[,msg ,?hl]] ,history {})))

(fn echom [msg ?hl]
  `(echo ,msg ,?hl true))

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
  (assert-compile (or (= (type modes) :string) (sequence? modes)) "modes should be a list or string" modes)
  (assert-compile (= (type from) :string) "from should be a string" from)
  (assert-compile (or (= nil ?opts) (table? ?opts)) "opts should be a table" ?opts)
  (let [form `(do)
        opts (collect [k v (pairs (or ?opts {}))]
               (values k v))
        modes (if (sequence? modes) modes [modes])
        to (match (type to)
             :string to
             _ (do
                 (set opts.desc (tostring (. to 2)))
                 (set opts.callback to)
                 ""))]
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

(fn autocmd* [bang ...]
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
    (let [cmd (: "autocmd%s %s %%s %s call v:lua.%s()"
                 :format
                 (if bang :! "")
                 events
                 (table.concat flags " ")
                 ns)]
      (table.insert form `(exec ,(if (list? pat)
                                     `(: ,cmd :format ,pat)
                                     (cmd:format pat)))))
    (when group
      (table.insert form `(exec "augroup END")))
    form))

(fn autocmd [...]
  (autocmd* false ...))

(fn autocmd! [...]
  (autocmd* true ...))

(fn augroup [group ...]
  (let [form `(do)]
    (each [_ au (pairs [...])]
      (when (= :autocmd (string.sub (tostring (. au 1)) 1 7))
        (table.insert au 2 group))
      (table.insert form au))
    form))

(fn command [cmd opts func]
  (assert-compile (table? opts) "opts should be a table" opts)
  (let [opts (collect [k v (pairs opts)]
               (values k v))
        bufnr (match opts.buffer
                n n
                true 0)]
    (set opts.bufnr nil)
    (if bufnr
        `(vim.api.nvim_buf_add_user_command ,bufnr ,cmd ,func ,opts)
        `(vim.api.nvim_add_user_command ,cmd ,func ,opts))))

(fn append! [str s]
  "Append to a string in place"
  (assert-compile (sym? str) "expected symbol name for str" str)
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
  `(or (= ,s nil) (= (next ,s) nil)))

(fn printf [s ...]
  `(print (: ,s :format ,...)))

{: setlocal
 : setlocal+=
 : setlocal^=
 : setlocal-=
 : exec
 : echo
 : echom
 : keymap
 : autocmd
 : autocmd!
 : augroup
 : command
 : append!
 : with-module
 : empty-or-nil?
 : printf}
