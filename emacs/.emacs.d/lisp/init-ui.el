;;; init-ui.el --- Customize UI elements

;; Disable tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;; Hide the splash screen and banner
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t)
  (custom-theme-set-faces 'atom-one-dark
   `(company-tooltip ((t (:background "#2c323c"))))
   `(company-tooltip-annotation ((t (:background "#2c323c"))))
   `(company-tooltip-common ((t (:background nil))))
   `(company-scrollbar-fg ((t (:background "#4b5363"))))
   `(company-scrollbar-bg ((t (:background "#2c323c"))))
   `(show-paren-match ((t (:background "#282c34" :foreground "#e06c75" :weight ultra-bold))))
   `(show-paren-mismatch ((t (:background "#e06c75" :foreground "#282c34" :weight ultra-bold))))
   `(cursor ((t (:background "#abb2bf" :foreground "#282c34"))))
   `(lazy-highlight ((t (:foreground "#282c34" :background "#e5c07b" :underline nil))))
   `(evil-ex-lazy-highlight ((t (:inherit 'lazy-highlight))))
   `(evil-ex-substitute-matches ((t (:inherit `evil-ex-search))))
   `(evil-ex-search ((t (:foreground "#e5c07b" :background "#5c6370")))))
)

(use-package doom-themes
  :disabled
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-org-config)
  (custom-set-faces
    `(mode-line ((t (:background "#21242b" :foreground "#9ca0a4" :box (:color "#202328" :line-width 1)))))
    `(mode-line-buffer-id ((t (:weight bold))))
    `(mode-line-emphasis ((t (:weight bold))))
    `(mode-line-inactive ((t (:background "#202328" :foreground "#5b6268" :box (:color "#202328" :line-width 1)))))
    `(lazy-highlight ((t (:foreground "#282c34" :background "#e5c07b" :underline nil))))
    `(evil-ex-lazy-highlight ((t (:inherit 'lazy-highlight))))
    `(evil-ex-substitute-matches ((t (:inherit `evil-ex-search))))
    `(evil-ex-search ((t (:foreground "#282c34" :background "#e5c07b")))))
)

(provide 'init-ui)
