(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (use-package evil-surround
    :ensure t
    :config
    (evil-surround-mode t))
  (use-package evil-commentary
    :ensure t
    :delight
    :config
    (evil-commentary-mode t))

  (defvar evil-leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")

  (define-key evil-normal-state-map "," evil-leader-map)
  (define-key evil-leader-map "b" 'switch-to-buffer)
  (define-key evil-leader-map "e" 'find-file)
  (define-key evil-leader-map "w" 'save-buffer)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-M-x)

  ;; Enable evil mode
  (evil-mode 1)
)

(provide 'init-evil)
