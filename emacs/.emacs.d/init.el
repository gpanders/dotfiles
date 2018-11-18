;; gpanders emacs init.el
;; References:
;;   [1] https://github.com/purcell/emacs.d

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

(setq use-package-always-ensure t) ; Always download package if not already installed
(use-package use-package-ensure-system-package)

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun load-user-init-file ()
  "Load the `user-init-file'."
  (interactive)
  (load-file user-init-file))

(defun toggle-relative-line-numbers ()
  "Toggle relative line numbers."
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers t)
    (setq display-line-numbers 'relative)))

;; Themes
(use-package dracula-theme
  ;; :disabled
  :config
  (load-theme 'dracula t))
(use-package leuven-theme
  :disabled
  :config
  (load-theme 'leuven t))
(use-package solarized-theme
  :disabled
  :config
  (load-theme 'solarized-dark t))

(use-package general ; Better keybindings
  :config
  ;; User ibuffer instead of list-buffers
  (general-def "C-x C-b" 'ibuffer))
(use-package delight)  ; Use delight to manage minor mode displays
(use-package magit)     ; Git front end
(use-package flx)  ; Fuzzy matching
(use-package smex) ; M-x enhancement for Emacs
(use-package key-chord ; Allow key-chords
  :config
  (key-chord-mode 1))
(use-package exec-path-from-shell ; Make sure Emacs PATH matches shell PATH
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_TYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
(use-package projectile ; Project management
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))
(use-package ivy ; Fast narrowing framewor
  :delight
  :bind ("C-s" . swiper)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'fullpath
        enable-recursive-minibuffers t
        projectile-completion-system 'ivy
        ivy-initial-inputs-alist '((Man-completion-table . "^")
                                   (woman ."^")))
  (ivy-mode 1))
(use-package counsel ; Use ivy completion for many common functions in Emacs
  :delight
  :config
  (setq counsel-mode-override-describe-bindings t)
  (counsel-mode 1))
(use-package rtags)
(use-package cmake-ide
  :after rtags
  :config
  (cmake-ide-setup))
(use-package markdown-mode
  :ensure-system-package pandoc
  :commands (markdown-mode gfm-mode)
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc")
  (when (eq system-type 'darwin)
    (setq markdown-open-command "/usr/local/bin/macdown")))
;; Evil mode!
(use-package evil
  :after general
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil)
  :config
  ;; Install evil packages
  (use-package evil-commentary
    :delight
    :config
    (evil-commentary-mode))
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))
  (use-package evil-magit
    :config
    (setq evil-magit-state 'normal
          evil-magit-use-y-for-yank nil))
  (use-package evil-collection
    :config
    (evil-collection-init))
  (use-package evil-unimpaired
    :load-path "site-lisp/evil-unimpaired"
    :config
    (evil-unimpaired-mode))

  ;; Create leader map
  (general-create-definer evil-leader-def
    :prefix ",")
  (evil-leader-def
    :keymaps 'normal
    "w" 'save-buffer
    "b" 'counsel-ibuffer
    "r" 'toggle-relative-line-numbers
    "ev" 'find-user-init-file
    "sv" 'load-user-init-file)

  (general-def 'normal
    "C-p" 'projectile-find-file
    "C-k" 'counsel-rg
    "-"   'dired-jump)

  (general-def '(normal visual)
    "C-y" 'yank
    "C-e" 'end-of-line
    "/"   'swiper)

  (general-define-key
   :keymaps 'evil-insert-state-map
   (general-chord "jk") 'evil-normal-state)

  ;; Enable evil mode
  (evil-mode 1))

;; Add parts of each file's directory to the buffer name if not unique [2]
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Open file in same place as we left it [2]
(require 'saveplace)
(setq-default save-place t)

;; Write backup files to own directory [2]
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Disable line numbers in certain modes
(dolist (m '(term-mode-hook shell-mode-hook eshell-mode-hook Custom-mode-hook dired-mode-hook))
  (add-hook m (lambda () (display-line-numbers-mode -1))))

;; Shorten yes or no prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; Mac specific options [1]
(when (eq system-type 'darwin)
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
