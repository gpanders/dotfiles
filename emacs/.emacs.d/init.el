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
(require 'init-os)
(require 'init-dired)

(use-package ccls
  :ensure t
  :if (executable-find "ccls")
  :after lsp-mode
  :config
  (setq ccls-executable (executable-find "ccls")))
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
  :init
  (setq company-idle-delay 0.2
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-code-other-buffers t
        company-require-match 'never
        company-global-modes '(not org-mode eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-transformers '(company-sort-by-occurrence))
  :config
  (use-package company-statistics
    :ensure t
    :config
    (company-statistics-mode))
  (global-company-mode))
(use-package cquery
  :ensure t
  :if (executable-find "cquery")
  :after lsp-mode
  :config
  (setq cquery-executable (executable-find "cquery")))
(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (undo-tree-mode nil "undo-tree"))))
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys))
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
(use-package htmlize
  :ensure t
  :commands (htmlize-file htmlize-buffer))
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
(use-package org
  :ensure t
  :demand
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb))
  :config
  (require 'init-org))
(use-package projectile
  :ensure t
  :bind-keymap (("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map))
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
        (cond ((executable-find "fd") "fd --type f --hidden --follow --exclude .git -0")
              ((executable-find "rg") "rg --files --hidden --glob '!.git' -0")
              ((executable-find "ag") "ag -g '' --hidden --ignore '.git' -0")
              (t projectile-generic-command)))
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (when (eq (projectile-project-vcs) 'git)
                (setq projectile-tags-file-name ".git/etags"
                      projectile-tags-command "git ls-files | ctags -e -L - -f \"%s\" %s"))))
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
(use-package smartparens
  :ensure t
  :delight
  :hook (prog-mode . smartparens-mode)
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



;; Disable some emacs prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Enable line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

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
