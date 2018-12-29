(use-package evil
  :ensure t
  :init
  (setq evil-default-state 'emacs)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(provide 'init-evil)
