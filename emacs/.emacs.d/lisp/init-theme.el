;; Themes
(use-package dracula-theme
  :disabled
  :ensure t
  :config
  (load-theme 'dracula t)
  (custom-set-faces
   '(line-number-current-line ((t (:weight bold :foreground "white" :inherit (line-number)))))))
(use-package leuven-theme
  :disabled
  :ensure t
  :config
  (load-theme 'leuven t))
(use-package solarized-theme
  ;; :disabled
  :ensure t
  :config
  (defun light ()
    "Activate light color theme."
    (interactive)
    (load-theme 'solarized-light t))
  (defun dark ()
    "Activate dark color theme."
    (interactive)
    (load-theme 'solarized-dark t))
  (load-theme 'solarized-dark t)
  (custom-set-faces
   '(line-number ((t (:background nil :inherit (highlight))))))
  (custom-set-faces
   '(line-number-current-line ((t (:weight bold :foreground nil :inherit (highlight default)))))))

(provide 'init-theme)
