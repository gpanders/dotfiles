;; gpanders emacs init.el
;; References:
;;   [1] https://github.com/purcell/emacs.d

;; Adjust garbage collection thresholds (adapted from [1])
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

;; Themes
(use-package dracula-theme
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

;; Install packages
(use-package delight)  ; Use delight to manage minor mode displays
(use-package key-chord ; Allow key-chords
  :config
  (key-chord-mode 1))
(use-package general) ; Better keybindings
(use-package exec-path-from-shell ; Make sure Emacs PATH matches shell PATH
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))
(use-package evil ; Evil mode!
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
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
    :custom
    (evil-magit-state 'normal)
    (evil-magit-use-y-for-yank nil))
  (use-package evil-collection
    :config
    (evil-collection-init))
  (use-package evil-unimpaired
    :load-path "user/evil-unimpaired"
    :config
    (evil-unimpaired-mode))
  (use-package evil-config ; Custom configuration
    :load-path "lisp")
  ;; Enable evil mode
  (evil-mode 1))
(use-package magit)     ; Git front end
(use-package projectile ; Project management
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))
(use-package flx)  ; Fuzzy matching
(use-package smex) ; M-x enhancement for Emacs
(use-package ivy ; Fast narrowing framework
  :delight
  :bind ("C-s" . swiper)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  (setq projectile-completion-system 'ivy))
(use-package counsel ; Use ivy completion for many common functions in Emacs
  :delight
  :config
  (counsel-mode 1))
(use-package smart-mode-line
  :disabled
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'light)
  :config
  (sml/setup))
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

;; Set frame size
;; (if (display-graphic-p)
;;     (progn
;;       (setq initial-frame-alist
;;             '((width . 160)
;;               (height . 50)))
;;       (setq default-frame-alist
;;             '((width . 160)
;;               (height . 50)))))

;; ;; Set default font
;; (set-face-attribute 'default nil
;;                     :family "Fira Code"
;;                     :height 120
;;                     :weight 'normal
;;                     :width 'normal)

;; Disable line numbers in certain modes
(dolist (m '(term-mode-hook shell-mode-hook eshell-mode-hook Custom-mode-hook dired-mode-hook))
  (add-hook m (lambda () (display-line-numbers-mode -1))))

;; Shorten yes or no prompt
(defalias 'yes-or-no-p 'y-or-n-p)

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


(when (eq system-type 'darwin)
  (advice-add 'handle-delete-frame :override
              #'handle-delete-frame-without-kill-emacs))

;; Start server if not already running
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(when (file-exists-p custom-file)
  (load custom-file))
