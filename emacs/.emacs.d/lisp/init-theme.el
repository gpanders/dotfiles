;; Themes
(use-package dracula-theme
  :disabled
  :custom-face
  (line-number-current-line ((t (:weight bold :foreground "white" :inherit (line-number)))))
  :config
  (load-theme 'dracula t))
(use-package leuven-theme
  :disabled
  :config
  (load-theme 'leuven t))
(use-package solarized-theme
  :disabled
  :custom-face
  (line-number ((t (:background nil :inherit (highlight)))))
  (line-number-current-line ((t (:weight bold :foreground nil :inherit (highlight default)))))
  :config
  (defun light ()
    "Activate light color theme."
    (interactive)
    (load-theme 'solarized-light t))
  (defun dark ()
    "Activate dark color theme."
    (interactive)
    (load-theme 'solarized-dark t))
  (load-theme 'solarized-dark t))
(use-package doom-themes
  :config
  ;; (load-theme 'doom-dracula t)
  ;; (load-theme 'doom-vibrant t)
  (load-theme 'doom-solarized-light t)
  ;; (load-theme 'doom-one t)
  )

(provide 'init-theme)
