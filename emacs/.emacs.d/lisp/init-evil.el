;;; init-evil.el --- Customize evil mode

(use-package evil
  :ensure t
  :custom
  (evil-search-module 'evil-search)
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (use-package evil-surround
    :ensure t
    :config
    (evil-surround-mode))
  (use-package evil-commentary
    :ensure t
    :delight
    :config
    (evil-commentary-mode))
  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1))
  (use-package evil-numbers
    :ensure t
    :bind (:map evil-normal-state-map
                ("C-a" . evil-numbers/inc-at-pt)
                ("C-c -" . evil-numbers/dec-at-pt)))
  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init))
  (use-package evil-visualstar
    :ensure t
    :config
    (global-evil-visualstar-mode))
  (use-package move-text :ensure t)

  (general-def 'motion
    ;; Unbind these in motion state since they often override other,
    ;; more useful bindings
    "DEL" nil
    "RET" nil
    "\\" nil
    "SPC" nil
    ;; Make window commands easier
    "C-w C-k" "C-w k"
    "C-w C-j" "C-w j"
    "C-w C-h" "C-w h"
    "C-w C-l" "C-w l"
    )

  ;; Evil bindings
  (general-def '(normal visual)
    "Q" 'evil-fill-and-move
    )

  (general-def 'normal
    "-" 'dired-jump
    ", w" 'save-buffer
    ", b" 'switch-to-buffer
    ", e" 'find-file
    ", g" 'magit-status
    "C-p" 'counsel-file-jump
    ;; (kbd "C-p") (lambda () (interactive) (ivy-read "Find file: " (directory-files-recursively default-directory "")))
    "[ b" 'previous-buffer
    "] b" 'next-buffer
    "[ q" 'flycheck-previous-error
    "] q" 'flycheck-next-error
    "[ e" 'move-text-up
    "] e" 'move-text-down
    "[ SPC" (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-above))))
    "] SPC" (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-below))))
    ;; paste above or below with newline
    "[ p" (lambda () (interactive) (evil-insert-newline-above) (evil-paste-after 1))
    "] p" (lambda () (interactive) (evil-insert-newline-below) (evil-paste-after 1))
    ;; select pasted text
    "g p" (kbd "` [ v ` ]")
    "\\\\" (lookup-key (current-global-map) (kbd "C-c k"))
    "DEL" 'evil-switch-to-windows-last-buffer
    )

  ;; Put space keys into an override map so that they are never
  ;; overriden by local bindings from other packages
  (general-def 'normal 'override
    :prefix "SPC"
    "SPC" 'counsel-M-x
    "u" 'universal-argument
    )


  (general-def 'visual
    "[ e" ":move'<--1"
    "] e" ":move'>+1"
    )

  ;; Exit insert mode with `jk'
  (general-define-key
   :states 'insert
   (general-chord "jk") 'evil-normal-state
   )

  ;; Enable evil mode
  (evil-mode 1)
)

(provide 'init-evil)
