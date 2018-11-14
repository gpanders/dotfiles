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

;; Vim keybindings
(evil-leader/set-leader ",")
(evil-leader/set-key
  "w" 'save-buffer
  "b" 'ivy-switch-buffer
  "r" 'toggle-relative-line-numbers
  "ev" 'find-user-init-file
  "sv" 'load-user-init-file)

(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "\C-e" 'end-of-line)
(define-key evil-normal-state-map "-" 'dired-jump)
(key-chord-define evil-insert-state-map (kbd "j k") 'evil-normal-state)
(key-chord-define evil-replace-state-map (kbd "j k") 'evil-normal-state)

(provide 'evil-config)
