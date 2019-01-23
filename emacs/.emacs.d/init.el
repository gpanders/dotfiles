;;; init.el --- Greg Anders (gpanders)'s emacs init.el
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Setup path before anything else
(use-package exec-path-from-shell
  :ensure t
  :if (memq system-type '(darwin gnu/linux))
  :config
  (setq exec-path-from-shell-arguments nil)
  (dolist (var '("http_proxy" "https_proxy" "no_proxy"))
    (push var exec-path-from-shell-variables))
  (exec-path-from-shell-initialize))

;; Custom lisp packages in ~/.emacs.d/lisp directory
(require 'init-backup)
(require 'init-ui)
(require 'init-evil)
(require 'init-org)
(require 'init-os)
(require 'init-python)

(use-package company
  :ensure t
  :delight
  :bind (:map company-active-map
              ("C-w" . nil)
              ("C-y" . company-complete)
              ("C-e" . company-abort)
              ("C-s" . company-filter-candidates)
              ("C-SPC" . company-complete-common)
              ([tab] . company-complete-common-or-cycle))
  :config
  (setq company-idle-delay nil
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-require-match 'never
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-transformers '(company-sort-by-occurrence))
  (use-package company-statistics
    :ensure t
    :config
    (company-statistics-mode))
  (use-package yasnippet
    :ensure t
    :config
    (delight 'yas-minor-mode nil "yasnippet")
    (push 'company-yasnippet company-backends)
    (yas-global-mode))
  (global-company-mode))
(use-package cquery
  :ensure t
  :config
  (setq cquery-executable (cond
                           ((eq system-type 'darwin) "/usr/local/bin/cquery")
                           ((eq system-type 'gnu/linux) "/usr/bin/cquery"))))
(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (undo-tree-mode nil "undo-tree"))))
(use-package flx
  :ensure t)
(use-package flycheck
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (push 'emacs-lisp-checkdoc flycheck-disabled-checkers)))
  (global-flycheck-mode))
(use-package general
  :ensure t)
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))
(use-package ivy
  :ensure t
  :delight
  :config
  ;; (setq enable-recursive-minibuffers t)
  (setq ivy-height 12
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        smex-completion-method 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (use-package ivy-hydra
    :ensure t)
  (use-package counsel
    :ensure t
    :delight
    :config
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c k") (cond ((executable-find "rg") 'counsel-rg)
                                        ((executable-find "ag") 'counsel-ag)
                                        ((executable-find "grep") 'counsel-grep)))
    (counsel-mode t))
  (ivy-mode t))
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         ;; (python-mode . lsp)
         (rust-mode . lsp))
  :config
  (setq lsp-prefer-flymake nil)
  ;; lsp-mode doesn't have a keymap so bind the keys locally when mode is
  ;; activated
  (add-hook 'lsp-mode-hook
            (lambda ()
              (progn
                (local-set-key (kbd "C-c C-f") 'lsp-format-buffer))))
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :init
    (setq lsp-ui-sideline-enable nil))
  (use-package company-lsp
    :ensure t
    :commands company-lsp))
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function #'ivy-completing-read))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (use-package pandoc-mode
    :ensure t
    :hook markdown-mode))
(use-package matlab-mode
  :ensure t
  :mode ("\\.m\\'" . matlab-mode))
(use-package projectile
  :ensure t
  :bind-keymap (("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map))
  :init
  (setq projectile-mode-line-prefix "Proj "
        projectile-mode-line-function #'(lambda () (format " Proj[%s]" (s-truncate 20 (projectile-project-name))))
        projectile-completion-system 'ivy
        projectile-project-search-path
        (cond ((eq system-type 'darwin) '("~/Projects"))
              ((eq system-type 'gnu/linux) '("~/work")))
        projectile-generic-command
        (cond ((executable-find "rg") "rg --files --hidden --glob '!.git' -0")
              ((executable-find "ag") "ag -g '' --hidden --ignore '.git' -0")
              (t projectile-generic-command)))
  :config
  (projectile-mode 1))
(use-package recentf
  :config
  ;; get rid of `find-file-read-only' and replace it with something
  ;; more useful
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)

  ;; enable recent files mode
  (recentf-mode t)
  (setq recentf-max-saved-items 50)

  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ivy-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting"))))
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (defvar cargo-run-args nil
    "Arguments to \"cargo-run\" command.")
  ;; (defun cargo-run-from-root-dir (command)
  ;;   "Run a command from Cargo root project directory."
  ;;   (catch 'wrong-major-mode
  ;;     (when (not (eq major-mode 'rust-mode))
  ;;    (throw 'wrong-major-mode "You must be in rust-mode to use this command"))
  ;;     (let ((cwd default-directory))
  ;;    (cd (locate-dominating-file buffer-file-name "Cargo.toml"))
  ;;    (eval command)
  ;;    (cd cwd))))
  (defun cargo-test ()
    "Run \"cargo test\"."
    (interactive)
    (compile "cargo test"))
  (defun cargo-run (args)
    "Run \"cargo run\"."
    (interactive
     (list
        (let ((args (eval cargo-run-args)))
        (if (or (not cargo-run-args) (car current-prefix-arg))
            (read-from-minibuffer "Arguments: " args)
            args))))
    (setq cargo-run-args args)
    (compile (concat "cargo run " args)))
  (defun cargo-build ()
      "Run \"cargo build\"."
    (interactive)
    (compile "cargo build"))
  (define-key rust-mode-map (kbd "C-c C-c t") 'cargo-test)
  (define-key rust-mode-map (kbd "C-c C-c r") 'cargo-run)
  (define-key rust-mode-map (kbd "C-c C-c b") 'cargo-build))
(use-package sane-term
  :disabled
  :ensure t
  :bind (("C-x t" . sane-term)
         ("C-x T" . sane-term-create)))
(use-package smartparens
  :ensure t)
(use-package smex
  :ensure t)
(use-package tcl-mode
  :mode ("\\.xdc\\'" . tcl-mode))
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;; Open eshell with a keybinding
(global-set-key (kbd "C-x t") 'eshell)

;; Enable dired-x
;; (load "dired-x")
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)
            ))

;; Disable some emacs prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Enable line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable auto-fill mode in text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Load customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(if (not (server-running-p))
    (server-start)
  )

;;; init.el ends here
