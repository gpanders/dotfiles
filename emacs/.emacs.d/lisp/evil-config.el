;; Evil mode configuration
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun load-user-init-file ()
  "Load the `user-init-file'."
  (interactive)
  (load-file user-init-file))

(defun toggle-relative-line-numbers ()
  "Toggle relative line numbers."
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers t)
    (setq display-line-numbers 'relative)))

;; Create leader map
(general-create-definer evil-leader-def
  :prefix ",")
(evil-leader-def
  :keymaps 'normal
  "w" 'save-buffer
  "b" 'ivy-switch-buffer
  "r" 'toggle-relative-line-numbers
  "ev" 'find-user-init-file
  "sv" 'load-user-init-file)

(general-def 'normal
  "-"   'dired-jump)

(general-def '(normal visual)
  "C-y" 'yank
  "C-e" 'end-of-line
  "/"   'swiper)

;; (general-def 'insert evil-insert-state-map
;;   (general-chord "jk") 'evil-normal-state)
(general-define-key
 :keymaps 'evil-insert-state-map
 (general-chord "jk") 'evil-normal-state)

(provide 'evil-config)
