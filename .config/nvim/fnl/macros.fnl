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

(fn autocmd* [clear ...]
  (let [args [...]
        group (if (sym? (. args 1)) (tostring (table.remove args 1)))
        event (table.remove args 1)
        pattern (if (or (= (type (. args 1)) :string) (sequence? (. args 1)))
                    (table.remove args 1))
        opts (if (table? (. args 1))
                 (collect [k v (pairs (table.remove args 1)) :into {: group}]
                   (values k v))
                 {: group})
        callback (if (and (= 1 (length args)) (not (list? (. args 1))))
                     (. args 1)
                     `(fn [] ,(unpack args)))
        form `(do)]
    (when clear
      (table.insert form `(each [_# {:id id#} (ipairs (vim.api.nvim_get_autocmds {:event ,(if (not= event "*") event)
                                                                                  :group ,(or opts.group _G.augroup)
                                                                                  :pattern ,pattern
                                                                                  :buffer ,opts.buffer}))]
                            (vim.api.nvim_del_autocmd id#))))
    (when (< 0 (length args))
      (when opts.group
        (table.insert form `(vim.api.nvim_create_augroup ,opts.group {:clear false})))
      (table.insert form `(vim.api.nvim_create_autocmd ,event {:group ,(or opts.group _G.augroup)
                                                               :pattern ,pattern
                                                               :callback ,callback
                                                               :buffer ,opts.buffer
                                                               :once ,opts.once
                                                               :nested ,opts.nested})))
    form))

(fn autocmd [...]
  (autocmd* false ...))

(fn autocmd! [...]
  (autocmd* true ...))

(fn augroup* [clear group ...]
  (set _G.augroup (tostring group))
  (if (and clear (= 0 (select :# ...)))
      `(vim.api.nvim_del_augroup_by_name ,(tostring group))
      `(do
        (vim.api.nvim_create_augroup ,(tostring group) {:clear ,clear})
        ,...)))

(fn augroup [group ...]
  (augroup* false group ...))

(fn augroup! [group ...]
  (augroup* true group ...))

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

(fn lazy-require [mod]
  `(setmetatable {} {:__index (fn [_# k#]
                                (. (require ,(tostring mod)) k#))}))

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
 : augroup!
 : command
 : append!
 : with-module
 : empty-or-nil?
 : printf
 : lazy-require}
