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

;; Install evil packages
(use-package evil-commentary
  :ensure t
  :delight
  :config
  (evil-commentary-mode))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package evil-magit
  :ensure t
  :config
  (setq evil-magit-state 'normal
        evil-magit-use-y-for-yank nil))
(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))
(use-package evil-unimpaired
  :load-path "site-lisp/evil-unimpaired"
  :config
  (evil-unimpaired-mode))

;; Create leader map
(with-eval-after-load 'general
  (progn
    (general-create-definer evil-leader-def
      :prefix ",")
    (evil-leader-def
      :keymaps 'normal
      "w" 'save-buffer
      "b" 'counsel-ibuffer
      "r" 'toggle-relative-line-numbers
      "ev" 'find-user-init-file
      "sv" 'load-user-init-file)
    (general-define-key
     :keymaps 'evil-insert-state-map
     (general-chord "jk") 'evil-normal-state)))

(provide 'init-evil)
