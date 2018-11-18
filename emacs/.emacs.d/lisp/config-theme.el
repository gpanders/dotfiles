;; Themes
(use-package dracula-theme
  ;; :disabled
  :config
  (load-theme 'dracula t))
(use-package leuven-theme
  :disabled
  :config
  (load-theme 'leuven t))
(use-package solarized-theme
  :disabled
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

(provide 'config-theme)
