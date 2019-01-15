;;; init-evil.el --- Customize evil mode

(use-package evil
  :ensure t
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

  ;; Configure default states for modes
  (dolist (var '(Custom-mode Info-mode))
    (delete var evil-normal-state-modes)
    (add-to-list 'evil-motion-state-modes var))

  (evil-define-key 'motion 'global
    ;; Unbind these in motion state since they often override other,
    ;; more useful bindings
    (kbd "DEL") nil
    (kbd "RET") nil
    (kbd "\\") nil
    (kbd "SPC") nil
    ;; Make window commands easier
    (kbd "C-w C-k") (lookup-key evil-motion-state-map (kbd "C-w k"))
    (kbd "C-w C-j") (lookup-key evil-motion-state-map (kbd "C-w j"))
    (kbd "C-w C-h") (lookup-key evil-motion-state-map (kbd "C-w h"))
    (kbd "C-w C-l") (lookup-key evil-motion-state-map (kbd "C-w l"))
  )

  ;; Evil bindings
  (evil-define-key '(normal visual) 'global "Q" 'evil-fill-and-move)
  (evil-define-key 'normal 'global
    (kbd "-") 'dired-jump
    (kbd ", w") 'save-buffer
    (kbd ", b") 'switch-to-buffer
    (kbd ", e") 'find-file
    (kbd ", g") 'magit-status
    (kbd "[ b") 'previous-buffer
    (kbd "] b") 'next-buffer
    (kbd "[ q") 'flycheck-previous-error
    (kbd "] q") 'flycheck-next-error
    (kbd "[ e") 'move-text-up
    (kbd "] e") 'move-text-down
    (kbd "[ SPC") (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-above))))
    (kbd "] SPC") (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-below))))
    ;; select pasted text
    (kbd "g p") (kbd "` [ v ` ]")
    ;; paste above or below with newline
    (kbd "[ p") (lambda () (interactive) (evil-insert-newline-above) (evil-paste-after 1))
    (kbd "] p") (lambda () (interactive) (evil-insert-newline-below) (evil-paste-after 1))
    (kbd "\\\\") (lookup-key (current-global-map) (kbd "C-c k"))
    (kbd "SPC u") 'universal-argument
    (kbd "SPC SPC") 'counsel-M-x
    (kbd "DEL") 'evil-switch-to-windows-last-buffer
  )

  (evil-define-key 'visual 'global
    (kbd "[ e") ":move'<--1"
    (kbd "] e") ":move'>+1")

  ;; Exit insert mode with `jk'
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

  ;; Use SPC as prefix for window commands (C-W by default)
  ;; Equivalent to nnoremap <Space> <C-W> in vim
  (dolist (var '("b" "c" "h" "j" "k" "l" "n" "o" "p" "q" "r" "s" "t" "w" "v" "H" "J" "K" "L"))
    (define-key evil-normal-state-map
      (kbd (concat "SPC " var))
      (lookup-key evil-motion-state-map (kbd (concat "C-w " var)))))

  ;; Enable evil mode
  (evil-mode 1)
  )

(provide 'init-evil)
