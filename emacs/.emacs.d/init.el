(setq custom-file (make-temp-file "custom"))

; Increase garbage collection threshold
(setq gc-cons-threshold 20000000)

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
(use-package dracula-theme)
(use-package leuven-theme)
(use-package solarized-theme)
(load-theme 'dracula t)

;; Install packages
(use-package better-defaults ; Better defaults for Emacs
  :config
  ;; Disable some options
  (ido-mode nil)
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore))

(use-package delight)  ; Use delight to manage minor mode displays
(use-package key-chord ; Allow key-chords
  :config
  (key-chord-mode 1))
(use-package general) ; Better keybindings
(use-package exec-path-from-shell ; Make sure Emacs PATH matches shell PATH
  :if (memq window-system '(mac ns x))
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
    :disabled
    :load-path "user/evil-unimpaired"
    :config
    (evil-unimpaired-mode))
  (use-package evil-config
    :ensure nil
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

;; Set default font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; Remove background from minibuffer prompt
(set-face-background 'minibuffer-prompt nil)

;; Enable line numbers but disable them in some modes
(global-display-line-numbers-mode)
(dolist (m '(term-mode-hook Custom-mode-hook))
  (add-hook m (lambda () (display-line-numbers-mode -1))))

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
(if (not (server-running-p)) (server-start))
