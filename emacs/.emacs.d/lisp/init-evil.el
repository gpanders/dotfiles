(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (use-package evil-surround
    :ensure t
    :config
    (evil-surround-mode))
  (use-package evil-commentary
    :ensure t
    :delight
    :config
    (evil-commentary-mode))
  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1))
  (use-package evil-numbers
    :ensure t
    :bind (:map evil-normal-state-map
	   ("C-c +" . evil-numbers/inc-at-pt)
	   ("C-c -" . evil-numbers/dec-at-pt)))
  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init))
  (use-package evil-visualstar
    :ensure t
    :config
    (global-evil-visualstar-mode))

  ;; Configure default states for modes
  (dolist (var '(Custom-mode Info-mode))
    (add-to-list 'evil-emacs-state-modes var))

  (defvar evil-leader-key ","
    "Leader key for Evil mode.")
  (defvar evil-leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")

  (define-key evil-motion-state-map evil-leader-key evil-leader-map)
  (define-key evil-leader-map evil-leader-key 'counsel-M-x)
  (define-key evil-leader-map "b" 'switch-to-buffer)
  (define-key evil-leader-map "e" 'find-file)
  (define-key evil-leader-map "w" 'save-buffer)
  (define-key evil-leader-map "g" 'magit-status)

  (evil-define-key '(normal visual) 'global "Q" 'evil-fill-and-move)
  (evil-define-key 'normal 'global "-" 'dired-jump)
  (evil-define-key 'normal 'global (kbd "C-w C-k") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "C-w C-j") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "C-w C-h") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "C-w C-l") 'evil-window-right)
  (evil-define-key '(motion normal) 'global (kbd "RET") nil)
  (evil-define-key '(motion normal) 'global (kbd "SPC") nil)
  (evil-define-key 'normal 'global (kbd "SPC u") 'universal-argument)

  ;; Enable evil mode
  (evil-mode 1)
)

(provide 'init-evil)
