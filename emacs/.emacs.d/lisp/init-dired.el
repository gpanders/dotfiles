;; Enable dired-x
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)


(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (setq dired-omit-files
                  (concat dired-omit-files "\\|^\\..+$"))
            ))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)
            (dired-hide-details-mode 1)
            ))

(provide 'init-dired)
