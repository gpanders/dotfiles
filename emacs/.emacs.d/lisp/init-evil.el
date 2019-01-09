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
  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init))

  (defvar evil-leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")

  (define-key evil-motion-state-map "," evil-leader-map)
  (define-key evil-leader-map "b" 'switch-to-buffer)
  (define-key evil-leader-map "e" 'find-file)
  (define-key evil-leader-map "w" 'save-buffer)

  (evil-define-key '(normal visual) 'global "Q" 'evil-fill-and-move)
  (evil-define-key 'normal 'global "-" 'dired-jump)
  (evil-define-key 'normal 'global (kbd "C-w C-k") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "C-w C-j") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "C-w C-h") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "C-w C-l") 'evil-window-right)

  ;; Enable evil mode
  (evil-mode 1)
)

(provide 'init-evil)
