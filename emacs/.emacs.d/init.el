;;; init.el
;;; References:
;;;   [1] https://github.com/purcell/emacs.d

;; Adjust garbage collection thresholds (adapted from [1])
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; Put customization in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Themes
(require 'config-theme)

(use-package anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (use-package company-anaconda
    :ensure t
    :after company
    :config
    (add-to-list 'company-backends '(company-anaconda :with company-capf))))
(use-package better-defaults
  :ensure t
  :config
  (ido-mode -1))
(use-package company
  :ensure t
  :config
  (global-company-mode))
(use-package cmake-ide
  :ensure t
  :after rtags
  :config
  (cmake-ide-setup))
(use-package delight ; Use delight to manage minor mode displays
  :ensure t)
(use-package dimmer
  :ensure t
  :config
  (dimmer-mode))
(use-package evil ; Evil mode!
  :ensure t
  :after general
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil)
  :config
  (require 'config-evil)
  (evil-mode 1))
(use-package exec-path-from-shell ; Make sure Emacs PATH matches shell PATH
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_TYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
(use-package flx ; Fuzzy matching
  :ensure t)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))
(use-package general ; Better keybindings
  :ensure t)
(use-package git-gutter
  :ensure t
  :delight
  :config
  (global-git-gutter-mode t))
(use-package ibuffer-vc
  :ensure t)
(use-package irony
  :ensure t
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :bind (:map irony-mode-map
         ([remap completion-at-point] . counsel-irony)
         ([remap complete-symbol] . counsel-irony))
  :config
  (use-package company-irony
    :ensure t
    :after company
    :config
    (add-to-list 'company-backends 'company-irony))
  (use-package flycheck-irony
    :ensure t
    :after flychec
    :hook (flycheck-mode . flycheck-irony-setup)))
(use-package ivy ; Fast narrowing framework
  :ensure t
  :delight
  :config
  (use-package swiper
    :ensure t
    :bind ("C-s" . swiper))
  (use-package counsel ; Use ivy completion for many common functions in Emacs
    :ensure t
    :delight
    :config
    (setq counsel-mode-override-describe-bindings t)
    (counsel-mode 1)
    (use-package counsel-projectile
      :ensure t
      :after projectile
      :config
      (counsel-projectile-mode)))
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'fullpath
        enable-recursive-minibuffers t
        projectile-completion-system 'ivy
        ivy-initial-inputs-alist '((Man-completion-table . "^")
                                   (woman ."^")))
  (ivy-mode 1))
(use-package key-chord ; Allow key-chords
  :ensure t
  :config
  (key-chord-mode 1))
(use-package magit ; Git front end
  :ensure t)
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc")
  (when (eq system-type 'darwin)
    (setq markdown-open-command "/usr/local/bin/macdown")))
(use-package projectile ; Project management
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-mode-line-prefix " Proj")
  (projectile-mode 1))
(use-package pyenv-mode
  :ensure t
  :hook (python-mode . pyenv-mode)
  :config
  (use-package pyenv-mode-auto
    :ensure t))
(use-package python
  :mode
  ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4))
(use-package rtags
  :ensure t)
(use-package smex ; M-x enhancement for Emacs
  :ensure t)

;; Disable line numbers in certain modes
(dolist (m '(term-mode-hook shell-mode-hook eshell-mode-hook Custom-mode-hook dired-mode-hook))
  (add-hook m (lambda () (display-line-numbers-mode -1))))

;; Set zero line spacing in term mode
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; Shorten yes or no prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; Mac specific options [1]
(when (eq system-type 'darwin)
  ;;
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-frame-parameter frame 'menu-bar-lines
                                   (if (display-graphic-p frame)
                                       1 0))))

  ;; Hide Emacs instead of killing it when last frame is closed
  (defun handle-delete-frame-without-kill-emacs (event)
    "Handle delete-frame events from the X server."
    (interactive "e")
    (let ((frame (posn-window (event-start event)))
          (i 0)
          (tail (frame-list)))
      (while tail
        (and (frame-visible-p (car tail))
             (not (eq (car tail) frame))
             (setq i (1+ i)))
        (setq tail (cdr tail)))
      (if (> i 0)
          (delete-frame frame t)
        ;; Not (save-buffers-kill-emacs) but instead:
        (ns-do-hide-emacs))))

  (advice-add 'handle-delete-frame :override
              #'handle-delete-frame-without-kill-emacs)

  ;; Set meta key to Command
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (with-eval-after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others))

;; Start server if not already running
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
