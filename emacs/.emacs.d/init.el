;;; init.el --- Greg Anders (gpanders)'s emacs init.el
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(require 'package)

(eval-and-compile
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

  (setq package-enable-at-startup nil)
  (package-initialize)

  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))

;; Setup path before anything else
(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (setq exec-path-from-shell-arguments nil)
  (dolist (var '("http_proxy" "https_proxy" "no_proxy"))
    (push var exec-path-from-shell-variables))
  (exec-path-from-shell-initialize))

;; Custom lisp packages in ~/.emacs.d/lisp directory
(require 'init-backup)
(require 'init-ui)
(require 'init-evil)
(require 'init-os)

(use-package company
  :ensure t
  :delight
  :bind (:map company-active-map
              ("C-w" . nil)
              ("C-y" . company-complete)
              ("C-e" . company-abort)
              ("C-s" . company-filter-candidates)
              ("C-SPC" . company-complete-common)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ([tab] . company-complete-common-or-cycle))
  :custom
  (company-idle-delay 0.2)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 2)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-code-other-buffers t)
  (company-require-match 'never)
  (company-global-modes '(not org-mode eshell-mode comint-mode erc-mode message-mode help-mode gud-mode))
  (company-transformers '(company-sort-by-occurrence))
  :config
  (use-package company-statistics
    :ensure t
    :config
    (company-statistics-mode))
  (global-company-mode))
(use-package cquery
  :ensure t
  :after lsp-mode
  :config
  (setq cquery-executable (executable-find "cquery")))
(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (undo-tree-mode nil "undo-tree"))))
(use-package flx
  :ensure t)
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (push 'emacs-lisp-checkdoc flycheck-disabled-checkers))))
(use-package git-gutter
  :ensure t
  :delight
  :config
  (global-git-gutter-mode +1))
(use-package ivy
  :ensure t
  :delight
  :config
  (setq ivy-height 12
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
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
    (global-set-key "\M-f" 'swiper)
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
    (setq lsp-ui-sideline-show-hover nil
          lsp-enable-snippet nil))
  (use-package company-lsp
    :ensure t
    :commands company-lsp
    :init
    (setq company-lsp-enable-snippet nil)))
    ;; :config
    ;; (use-package yasnippet
    ;;   :ensure t
    ;;   :delight yas-minor-mode
    ;;   :config
    ;;   (push 'company-yasnippet company-backends)
    ;;   (yas-global-mode))))
(use-package magit
  :ensure t
  :hook (with-editor-mode . evil-insert-state)
  :config
  (setq magit-completing-read-function #'ivy-completing-read))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :config
  (use-package pandoc-mode
    :ensure t
    :hook markdown-mode))
(use-package matlab-mode
  :ensure t
  :mode ("\\.m\\'" . matlab-mode))
(use-package persp-mode
  :ensure t
  :demand
  :bind (:map evil-normal-state-map
         ("g t" . persp-next)
         ("g T" . persp-prev)
         ("g s" . persp-frame-switch))
  :init
  (setq persp-keymap-prefix (kbd "C-x p"))
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-auto-save-opt 1
        persp-nil-hidden t
        persp-nil-name "nil")
  (defun persp-uncontained-buffer-p (buffer)
    (not (persp-contain-buffer-p buffer)))
  (add-to-list 'ivy-ignore-buffers #'persp-uncontained-buffer-p)
  (use-package persp-mode-projectile-bridge
    :ensure t
    :after (persp-mode projectile)
    :config
    (add-hook 'persp-mode-projectile-bridge-mode-hook
              (lambda ()
                  (if persp-mode-projectile-bridge-mode
                      (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                    (persp-mode-projectile-bridge-kill-perspectives))))
    (add-hook 'projectile-mode-hook #'persp-mode-projectile-bridge-mode))
  (persp-mode 1))
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)
         ("\C-cb" . org-switchb))
  :init
  (defvar org-gtd-directory nil "Location of GTD org mode files.")

  (setq org-gtd-directory
        (cond ((eq system-type 'darwin) "~/Documents/Notes/gtd/")
              ((eq system-type 'gnu/linux) "~/notes/gtd/")))
  (defun open-gtd-inbox ()
    "Open GTD inbox."
    (interactive)
    (find-file (expand-file-name "inbox.org" org-gtd-directory)))

  (defun open-gtd-actions ()
    "Open GTD next actions list."
    (interactive)
    (find-file (expand-file-name "actions.org" org-gtd-directory)))

  (defun open-gtd-projects ()
    "Open GTD projects list."
    (interactive)
    (find-file (expand-file-name "projects.org" org-gtd-directory)))

  (defun open-gtd-tickler ()
    "Open GTD tickler."
    (interactive)
    (find-file (expand-file-name "tickler.org" org-gtd-directory)))

  (defun open-gtd-someday-maybe ()
    "Open GTD someday/maybe list."
    (interactive)
    (find-file (expand-file-name "someday.org" org-gtd-directory)))
  :config
  (require 'init-org))
(use-package projectile
  :ensure t
  :bind-keymap (("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map))
  :bind (([remap evil-jump-to-tag] . projectile-find-tag)
         ([remap find-tag] . projectile-find-tag))
  :init
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-mode-line-prefix " Proj"
        projectile-mode-line-function (lambda () (format " Proj[%s]" (s-truncate 20 (projectile-project-name))))
        projectile-completion-system 'ivy
        projectile-require-project-root nil
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  :config
  (setq projectile-generic-command
        (cond ((executable-find "rg") "rg --files --hidden --glob '!.git' -0")
              ((executable-find "ag") "ag -g '' --hidden --ignore '.git' -0")
              (t projectile-generic-command)))
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (when (eq (projectile-project-vcs) 'git)
                (setq projectile-tags-file-name ".git/etags"
                      projectile-tags-command "git ls-files | ctags -e -L - -f \"%s\" %s"))))
  (use-package counsel-projectile
    :ensure t
    :after counsel
    :config
    (counsel-projectile-mode))
  (projectile-mode 1))
(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.pyx\\'" . python-mode))
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map
              ("C-c C-y" . run-ipython))
  :init
  (require 'init-python))
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))
(use-package sane-term
  :ensure t
  :bind (("C-x t" . sane-term)
         ("C-x T" . sane-term-create)))
(use-package smartparens
  :ensure t
  :delight
  :config
  (require 'smartparens-config)
  (sp-local-pair 'python-mode "'" nil
                 :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p))
  (dolist (char '("{" "(" "["))
    (sp-local-pair 'prog-mode char nil :post-handlers '(("||\n[i]" "RET"))))
  (sp-local-pair '(c-mode c++-mode rust-mode)
                 "/*" "*/" :post-handlers '(("* ||\n[i]" "RET")))
  (setq sp-cancel-autoskip-on-backward-movement nil))
(use-package smex
  :ensure t)
(use-package tcl-mode
  :mode ("\\.xdc\\'" . tcl-mode))
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-engine 'luatex))
(use-package vhdl-mode
  :mode (("\\.vhd\\'" . vhdl-mode)
         ("\\.vhdl\\'" . vhdl-mode))
  :config
  (setq vhdl-intelligent-tab nil
        vhdl-standard (quote (8 nil))))

;; Use simple shell for inferior shells
(when (memq system-type '(darwin gnu/linux))
    (setq shell-file-name "/bin/sh"))

;; Enable dired-x
;; (load "dired-x")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Disable some emacs prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Enable line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'smartparens-mode)

;; Enable auto-fill mode in text modes
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Load customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'server)
(unless (server-running-p)
    (server-start))

;; Set garbage collection setings back to reasonable values
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 16777216
                           gc-cons-percentage 0.1)))
;;; init.el ends here
