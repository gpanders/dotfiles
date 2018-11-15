(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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
(load-theme 'solarized-dark t)

;; Better defaults for Emacs
(use-package better-defaults
  :config
  ;; Disable some options
  (ido-mode nil)
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore))

;; Use delight to manage minor mode displays
(use-package delight)

;; Allow key-chords
(use-package key-chord
  :config
  (key-chord-mode 1))

;; Better keybindings
(use-package general)

;; Make sure Emacs PATH matches shell PATH
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Evil mode!
(use-package evil
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

;; Git porcelain
(use-package magit
  :ensure-system-package git)

;; Package management
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

;; Fuzzy matching
(use-package flx)

;; Similar to ido or helm
(use-package ivy
  :delight
  :bind ("C-s" . swiper)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  (setq projectile-completion-system 'ivy))

;; Includes ivy keybindings
(use-package counsel
  :delight
  :config
  (counsel-mode 1))

;; Smart mode line
(use-package smart-mode-line
  :disabled
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'light)
  :config
  (sml/setup))

;; Markdown mode
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

;; Disable line numbers in terminal
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Start server if not already running
(if (not (server-running-p)) (server-start))
