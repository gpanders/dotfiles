(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun load-user-init-file ()
  "Load the `user-init-file'."
  (interactive)
  (load-file user-init-file))

(defun toggle-relative-line-numbers ()
  "Toggle relative line numbers."
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers t)
    (setq display-line-numbers 'relative)))

(use-package evil ; Evil mode!
  :init
  (setq evil-want-C-u-scroll t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        evil-symbol-word-search t
        shift-select-mode nil
        evil-want-keybinding nil)
  :config
  ;; Default modes
  (dolist (mode '(tabulated-list-mode view-mode comint-mode term-mode calendar-mode Man-mode grep-mode))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode '(help-mode debugger-mode))
    (evil-set-initial-state mode 'normal))

  ;; Install evil packages
  (use-package evil-args
    :commands (evil-inner-arg evil-outer-arg
               evil-forward-arg evil-backward-arg
               evil-jump-out-args))
  (use-package evil-indent-plus
    :commands (evil-indent-plus-i-indent
               evil-indent-plus-a-indent
               evil-indent-plus-i-indent-up
               evil-indent-plus-a-indent-up
               evil-indent-plus-i-indent-up-down
               evil-indent-plus-a-indent-up-down))
  (use-package evil-escape
    :commands evil-escape-mode
    :hook (after-init . evil-escape-mode)
    :init
    (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
          evil-escape-excluded-major-modes '(neotree-mode)
          evil-escape-key-sequence "jk"
          evil-escape-delay 0.25)
    :config
    (push #'minibufferp evil-escape-inhibit-functions))
  (use-package evil-matchit
    :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
    :bind ([remap evil-jump-item] . evilmi-jump-items)
    :config
    (add-hook 'python-mode-hook (lambda () (setq-local evilmi-always-simple-jump t)))
    (global-evil-matchit-mode 1))
  (use-package evil-commentary
    :delight
    :config
    (evil-commentary-mode))
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))
  (use-package evil-magit
    :config
    (setq evil-magit-state 'normal
          evil-magit-use-y-for-yank nil))
  (use-package evil-collection
    :config
    (evil-collection-init))
  (use-package evil-unimpaired
    :disabled
    :load-path "site-lisp/evil-unimpaired"
    :config
    (evil-unimpaired-mode))

  ;; Create leader map
  (use-package evil-leader
    :config
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "w" 'save-buffer
      "b" 'switch-to-buffer
      "r" 'toggle-relative-line-numbers
      "ev" 'find-user-init-file
      "sv" 'load-user-init-file)
    (global-evil-leader-mode 1))

  (evil-global-set-key 'normal "-" 'dired-jump)

  (evil-mode 1))

(provide 'init-evil)
