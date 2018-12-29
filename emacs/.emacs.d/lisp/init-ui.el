;; Disable tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Hide the splash screen and banner
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))

(provide 'init-ui)
