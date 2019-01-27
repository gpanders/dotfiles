;;; init-ui.el --- Customize UI elements

;; Disable tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;; Hide the splash screen and banner
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(defvar my-dark-theme nil)
(defvar my-light-theme nil)
(defun toggle-dark-light-theme ()
  "Switch between `my-dark-theme' and `my-light-theme'"
  (interactive)
  (if (memq my-light-theme custom-enabled-themes)
      (progn
        (disable-theme my-light-theme)
        (load-theme my-dark-theme t))
    (progn
      (disable-theme my-dark-theme)
      (load-theme my-light-theme t))))

(use-package atom-one-dark-theme
  :disabled
  :ensure t
  :config
  (setq my-dark-theme 'atom-one-dark)
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
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq my-light-theme 'doom-tomorrow-day
        my-dark-theme 'doom-one)
  (load-theme 'doom-one t)
  (doom-themes-org-config)
  (custom-theme-set-faces 'doom-one
    `(mode-line ((t (:background "#21242b" :foreground "#9ca0a4" :box (:color "#202328" :line-width 1)))))
    `(mode-line-buffer-id ((t (:weight bold))))
    `(mode-line-emphasis ((t (:weight bold))))
    `(mode-line-inactive ((t (:background "#202328" :foreground "#5b6268" :box (:color "#202328" :line-width 1)))))
    `(lazy-highlight ((t (:foreground "#282c34" :background "#e5c07b" :underline nil))))
    `(evil-ex-lazy-highlight ((t (:inherit 'lazy-highlight))))
    `(evil-ex-substitute-matches ((t (:inherit `evil-ex-search))))
    `(evil-ex-search ((t (:foreground "#282c34" :background "#e5c07b")))))
)

;; Light theme alternative
(use-package leuven-theme
  :disabled
  :ensure t
  :config
  (setq my-light-theme 'leuven))

(use-package color-theme-sanityinc-tomorrow
  :disabled
  :ensure t
  :config
  (setq my-light-theme 'sanityinc-tomorrow-day))

;; Load theme (dark by default)
(load-theme my-dark-theme t)

(provide 'init-ui)
