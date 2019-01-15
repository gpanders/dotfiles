;;; init-evil.el --- Customize evil mode

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
    (delete var evil-normal-state-modes)
    (add-to-list 'evil-motion-state-modes var))

  (defvar evil-leader-key ","
    "Leader key for Evil mode.")
  (defvar evil-leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")

  (define-key evil-motion-state-map evil-leader-key evil-leader-map)
  (define-key evil-leader-map "b" 'switch-to-buffer)
  (define-key evil-leader-map "e" 'find-file)
  (define-key evil-leader-map "w" 'save-buffer)
  (define-key evil-leader-map "g" 'magit-status)

  (evil-define-key 'motion 'global
    (kbd "DEL") nil
    (kbd "RET") nil
    (kbd "SPC") nil)

  (evil-define-key '(normal visual) 'global "Q" 'evil-fill-and-move)
  (evil-define-key 'normal 'global
    "-" 'dired-jump
    (kbd "C-w C-k") 'evil-window-up
    (kbd "C-w C-j") 'evil-window-down
    (kbd "C-w C-h") 'evil-window-left
    (kbd "C-w C-l") 'evil-window-right
    (kbd "SPC k") 'evil-window-up
    (kbd "SPC j") 'evil-window-down
    (kbd "SPC h") 'evil-window-left
    (kbd "SPC l") 'evil-window-right
    (kbd "SPC u") 'universal-argument
    (kbd "RET") 'counsel-M-x
    (kbd "DEL") 'evil-switch-to-windows-last-buffer)

  ;; Enable evil mode
  (evil-mode 1)
)

(provide 'init-evil)
