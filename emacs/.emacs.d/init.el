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

(require 'init-os)
(require 'init-ui)
(require 'init-evil)
(require 'init-org)

(use-package company
  :ensure t
  :delight
  :config
  (setq company-idle-delay 0.1)
  (setq company-dabbrev-downcase nil)
  (global-company-mode))
(use-package cquery
  :ensure t
  :config
  (setq cquery-executable (cond
                           ((eq system-type 'darwin) "/usr/local/bin/cquery")
                           ((eq system-type 'gnu/linux) "/usr/bin/cquery"))))
(use-package delight
  :ensure t)
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))
(use-package flx
  :ensure t)
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))
(use-package ivy
  :ensure t
  :delight
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (use-package counsel
    :ensure t
    :delight
    :config
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c k") (cond ((executable-find "rg") 'counsel-rg)
					((executable-find "ag") 'counsel-ag)
					((exectuable-find "grep") 'counsel-grep)))
    (counsel-mode t))
  (ivy-mode t))
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode . lsp)
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
    :commands company-lsp
    :config
    (use-package yasnippet
      :ensure t
      :delight
      :config
      (yas-global-mode 1))))
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read))
(use-package projectile
  :ensure t
  :bind-keymap (("s-p" . projectile-command-map)
		("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/Development" "~/Projects"))
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1))
(use-package recentf
  :ensure nil
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
  (defcustom cargo-run-args nil
    "Arguments to \"cargo-run\" command."
    :type 'string
    :group 'cargo)
  (defun cargo-run-from-root-dir (command)
    "Change \"default-directory\" to Cargo root project directory."
    (catch 'wrong-major-mode
      (when (not (eq major-mode 'rust-mode))
	(throw 'wrong-major-mode "You must be in rust-mode to use this command"))
      (let ((cwd default-directory))
	(cd (locate-dominating-file default-directory "Cargo.toml"))
	(eval command)
	(cd cwd))))
  (defun cargo-test ()
    "Run \"cargo test\""
    (interactive)
    (cargo-run-from-root-dir '(compile "cargo test")))
  (defun cargo-run (args)
    "Run \"cargo run\""
    (interactive
     (list
	(let ((args (eval cargo-run-args)))
	(if (or (not cargo-run-args) (car current-prefix-arg))
	    (read-from-minibuffer "Arguments: " args)
	    args))))
    (setq cargo-run-args args)
    (cargo-run-from-root-dir '(compile (concat "cargo run " args))))
  (defun cargo-build ()
      "Run \"cargo build\""
    (interactive)
    (cargo-run-from-root-dir '(compile "cargo build")))
  (define-key rust-mode-map (kbd "C-c C-c t") 'cargo-test)
  (define-key rust-mode-map (kbd "C-c C-c r") 'cargo-run)
  (define-key rust-mode-map (kbd "C-c C-c b") 'cargo-build))
(use-package sane-term
  :ensure t
  :bind (("C-x t" . sane-term)
	 ("C-x T" . sane-term-create)))
(use-package smex
  :ensure t)
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;; Show matching parens
(show-paren-mode t)

;; Disable some emacs prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

;; Enable line numbers
(global-display-line-numbers-mode)
;; ...but disable them in some modes
(dolist (hook '(help-mode-hook term-mode-hook compilation-mode-hook Custom-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

;; Enable auto-revert mode globally
(global-auto-revert-mode t)

;; Load customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
