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
  :init
  (setq company-idle-delay nil
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-require-match 'never
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
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
(use-package general
  :ensure t)
(use-package git-gutter
  :ensure t
  :delight
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
  :config
  (use-package pandoc-mode
    :ensure t
    :hook markdown-mode))
(use-package matlab-mode
  :ensure t
  :mode ("\\.m\\'" . matlab-mode))
(use-package persp-mode
  :ensure t
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-auto-save-fname "autosave"
        persp-auto-save-opt 1
        persp-nil-hidden t
        persp-nil-name "nil"))
(use-package projectile
  :ensure t
  :bind-keymap (("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map))
  :bind (([remap evil-jump-to-tag] . projectile-find-tag)
         ([remap find-tag] . projectile-find-tag))
  :init
  (setq projectile-mode-line-prefix " Proj"
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
  (setq projectile-require-project-root nil)
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (when (eq (projectile-project-vcs) 'git)
                (setq projectile-tags-file-name ".git/etags"
                      projectile-tags-command "git ls-files | ctags -e -L - -f \"%s\" %s"))))
  (projectile-mode 1))
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (with-eval-after-load 'projectile
    (projectile-register-project-type 'rust-cargo '("Cargo.toml")
                                      :compile "cargo build"
                                      :test "cargo test"
                                      :run "cargo run")))
(use-package sane-term
  :ensure t
  :bind (("C-x t" . sane-term)
         ("C-x T" . sane-term-create)))
(use-package smartparens
  :ensure t
  :delight
  :config
  (sp-with-modes 'emacs-lisp-mode (sp-local-pair "'" nil :actions nil))
  (smartparens-global-mode))
(use-package smex
  :ensure t)
(use-package tcl-mode
  :mode ("\\.xdc\\'" . tcl-mode))
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

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
