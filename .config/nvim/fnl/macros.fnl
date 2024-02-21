(local M {})

(fn M.echo [msg ?hl ?history]
  (let [history (not (not ?history))]
    `(vim.api.nvim_echo [[,msg ,?hl]] ,history {})))

(fn M.echom [msg ?hl]
  `(echo ,msg ,?hl true))

(fn M.keymap [modes from to ?opts]
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
                 (when (= nil opts.desc)
                   (if (sym? to)
                       (set opts.desc (tostring to))
                       (set opts.desc (tostring (. to 2)))))
                 (set opts.callback to)
                 ""))]
    (when (= opts.noremap nil)
      (set opts.noremap true))
    (when (and opts.expr (= opts.replace_keycodes nil))
      (set opts.replace_keycodes true))
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
        pattern (if (or (= (type (. args 1)) :string) (sym? (. args 1)) (sequence? (. args 1)))
                    (table.remove args 1))
        opts (if (table? (. args 1))
                 (collect [k v (pairs (table.remove args 1)) &into {: group}]
                   (values k v))
                 {: group})
        desc (if (and (= (type (. args 1)) :string)
                      (< 1 (length args)))
                 (table.remove args 1))
        callback (. args 1)
        form `(do)]
    (when clear
      (table.insert form (if (= event "*")
                             `(each [_# v# (ipairs (vim.api.nvim_get_autocmds {:group ,(or opts.group _G.augroup)
                                                                               :pattern ,pattern
                                                                               :buffer ,opts.buffer}))]
                                (vim.api.nvim_del_autocmd v#.id))
                             `(vim.api.nvim_clear_autocmds {:event ,event
                                                            :group ,(or opts.group _G.augroup)
                                                            :pattern ,pattern
                                                            :buffer ,opts.buffer}))))
    (when (< 0 (length args))
      (when (and opts.group (not clear))
        (table.insert form `(vim.api.nvim_create_augroup ,opts.group {:clear false})))
      (table.insert form `(vim.api.nvim_create_autocmd ,event {:group ,(or opts.group _G.augroup)
                                                               :pattern ,pattern
                                                               :desc ,(or desc (if (sym? callback) (tostring callback)))
                                                               :command ,(if (= (type callback) :string) callback)
                                                               :callback ,(if (not= (type callback) :string) callback)
                                                               :buffer ,opts.buffer
                                                               :once ,opts.once
                                                               :nested ,opts.nested})))
    ; Do not return anything from the autocmd macro
    (table.insert form `nil)

    form))

(fn M.autocmd [...]
  (autocmd* false ...))

(fn M.autocmd! [...]
  (autocmd* true ...))

(fn augroup* [clear group ...]
  (set _G.augroup (tostring group))
  (if (and clear (= 0 (select :# ...)))
      `(vim.api.nvim_del_augroup_by_name ,(tostring group))
      `(do
        (vim.api.nvim_create_augroup ,(tostring group) {:clear ,clear})
        ,...)))

(fn M.augroup [group ...]
  (augroup* false group ...))

(fn M.augroup! [group ...]
  (augroup* true group ...))

(fn M.command [cmd opts func]
  (assert-compile (table? opts) "opts should be a table" opts)
  (let [opts (collect [k v (pairs opts)]
               (values k v))
        bufnr (match opts.buffer
                n n
                true 0)]
    (set opts.bufnr nil)
    (if bufnr
        `(vim.api.nvim_buf_create_user_command ,bufnr ,cmd ,func ,opts)
        `(vim.api.nvim_create_user_command ,cmd ,func ,opts))))

(fn M.with-module [module-binding ...]
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

(fn M.printf [s ...]
  `(print (: ,s :format ,...)))

(fn M.silent [...]
  `(let [p# _G.print]
     (tset _G :print #nil)
     ,...
     (tset _G :print p#)))

(fn M.exec [cmd]
  `(let [_# (vim.api.nvim_exec2 ,cmd {})] nil))

M
