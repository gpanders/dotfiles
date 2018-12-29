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

(use-package company
  :ensure t)
(use-package cquery
  :ensure t
  :config
  (setq cquery-executable (cond
                           ((eq system-type 'darwin) "/usr/local/bin/cquery")
                           ((eq system-type 'gnu/linux) "/usr/bin/cquery"))))
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))
(use-package flx-ido
  :ensure t
  :config
  (setq ido-use-faces nil)
  (flx-ido-mode 1))
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (prog-mode . lsp)
  :config
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)
  (use-package company-lsp
    :ensure t
    :commands company-lsp))
(use-package magit
  :ensure t)
(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/Development" "~/Projects"))
  (projectile-mode 1))
(use-package recentf
  :ensure nil
  :config
  ;; get rid of `find-file-read-only' and replace it with something
  ;; more useful
  (global-set-key (kbd "C-x C-r") 'ido-recentf-open)

  ;; enable recent files mode
  (recentf-mode t)
  (setq recentf-max-saved-items 50)

  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
	(message "Opening file...")
      (message "Aborting"))))
(use-package smex
  :ensure t
  :bind ("M-x" . smex))

;; Enable ido mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

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
