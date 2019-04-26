;;; init-evil.el --- Customize evil mode

(use-package evil
  :ensure t
  :custom
  (evil-search-module 'evil-search)
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-ex-search-vim-style-regexp t)
  :config
  (use-package evil-surround
    :ensure t
    :commands evil-surround-mode
    :hook (evil-mode . global-evil-surround-mode))
  (use-package evil-commentary
    :ensure t
    :delight
    :commands evil-commentary-mode
    :hook (evil-mode . evil-commentary-mode))
  (use-package evil-matchit
    :ensure t
    :commands evil-matchit-mode
    :hook (evil-mode . global-evil-matchit-mode))
  (use-package evil-numbers
    :ensure t
    :bind (:map evil-normal-state-map
                ("C-a" . evil-numbers/inc-at-pt)
                ("C-c -" . evil-numbers/dec-at-pt)))
  (use-package evil-collection
    :ensure t
    :functions evil-collection-init
    :config
    (evil-collection-init))
  (use-package evil-visualstar
    :ensure t
    :commands evil-visualstar-mode
    :hook (evil-mode . global-evil-visualstar-mode))
  (use-package move-text
    :commands (move-text-up move-text-down)
    :ensure t)

  ;; Fixes error message:
  ;;   Error in post-command-hook (evil-repeat-post-hook): (wrong-type-argument number-or-marker-p nil)
  ;; See https://github.com/company-mode/company-mode/issues/383
  (evil-declare-change-repeat 'company-complete)

  (defun find-file-in-path ()
    "Find files recursively from current directory."
    (interactive)
    (let ((find-program (cond ((executable-find "rg") "rg")
                              ((executable-find "ag") "ag")
                              (t find-program)))
          (counsel-file-jump-args (cond ((executable-find "rg") "--files --hidden --glob '!.git'")
                                        ((executable-find "ag") "-g '' --hidden --ignore '.git'")
                                        (t counsel-file-jump-args))))
      (counsel-file-jump)))

  (defun my-evil-backward-delete-line ()
    "Kill line backward."
    (interactive)
    (if (looking-back "^" 0)
        (backward-delete-char 1)
      (if (looking-back "^\s*" 0)
          (delete-region (point) (line-beginning-position))
        (evil-delete (+ (line-beginning-position) (current-indentation)) (point)))))

  (evil-define-key 'motion 'global
    ;; Unbind these in motion state since they often override other,
    ;; more useful bindings
    (kbd "DEL") nil
    "\\" nil
    (kbd "SPC") nil
    ;; Make window commands easier
    (kbd "C-w C-k") (kbd "C-w k")
    (kbd "C-w C-j") (kbd "C-w j")
    (kbd "C-w C-h") (kbd "C-w h")
    (kbd "C-w C-l") (kbd "C-w l")
  )

  ;; Normal + Visual
  (evil-define-key '(normal visual) 'global
    "Q" 'evil-fill-and-move
    (kbd "C-l") 'evil-ex-nohighlight
    )

  ;; Normal only
  (evil-define-key 'normal 'global
    (kbd "-") 'dired-jump
    (kbd ", w") 'save-buffer
    (kbd ", b") 'switch-to-buffer
    (kbd ", e") 'find-file
    (kbd "C-p") 'find-file-in-path
    (kbd "\\ g") 'magit-status
    ;; Map & to :&& in normal mode (repeat last substitution with flags)
    (kbd "&") 'evil-ex-repeat-substitute-with-flags
    (kbd "[ b") 'previous-buffer
    (kbd "] b") 'next-buffer
    (kbd "[ q") 'flycheck-previous-error
    (kbd "] q") 'flycheck-next-error
    (kbd "[ e") 'move-text-up
    (kbd "] e") 'move-text-down
    (kbd "[ SPC") (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-above))))
    (kbd "] SPC") (lambda (count) (interactive "p") (dotimes (_ count) (save-excursion (evil-insert-newline-below))))
    ;; paste above or below with newline
    (kbd "[ p") (lambda () (interactive) (evil-insert-newline-above) (evil-paste-after 1))
    (kbd "] p") (lambda () (interactive) (evil-insert-newline-below) (evil-paste-after 1))
    ;; select pasted text
    (kbd "g p") (kbd "` [ v ` ]")
    (kbd "\\\\") (lookup-key (current-global-map) (kbd "C-c k"))
    )

  ;; Visual only
  (evil-define-key 'visual 'global
    (kbd "[ e") ":move'<--1"
    (kbd "] e") ":move'>+1"
    )

  ;; Put space keys into an override map so that they are never
  ;; overriden by local bindings from other packages
  (evil-define-key '(normal motion) 'global
    (kbd "SPC SPC") 'counsel-M-x
    (kbd "SPC u") 'universal-argument
    )

  ;; Insert mode
  (evil-define-key 'insert 'global
    (kbd "C-SPC") 'company-complete
    (kbd "C-x C-f") 'company-files
    (kbd "C-x C-o") 'company-capf
    (kbd "C-x C-s") 'company-yasnippet
    (kbd "C-x s") 'company-ispell
    (kbd "C-d") 'evil-delete-char
    (kbd "C-u") 'my-evil-backward-delete-line
    )

  ;; Exit insert/replace mode with `jk'
  (with-eval-after-load 'key-chord
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-replace-state-map "jk" 'evil-normal-state))

  ;; Enable evil mode
  (evil-mode 1)
  )

(provide 'init-evil)
