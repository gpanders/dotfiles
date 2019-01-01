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

(use-package auctex
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))
(use-package company
  :ensure t
  :delight
  :config
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
	 (python-mode . lsp))
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
    :commands lsp-ui-mode)
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
(use-package sane-term
  :ensure t
  :bind (("C-x t" . sane-term)
	 ("C-x T" . sane-term-create)))
(use-package smex
  :ensure t)

;; Show matching parens
(show-paren-mode t)

;; Disable some emacs prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

;; Enable auto-revert mode globally
(global-auto-revert-mode t)

;; Load customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
