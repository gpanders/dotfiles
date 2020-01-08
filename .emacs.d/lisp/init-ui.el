;;; init-ui.el --- Customize UI elements

;; Disable tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;; Hide the splash screen and banner
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(defvar my-dark-theme nil)
(defvar my-light-theme nil)
(defun toggle-dark-light-theme ()
  "Switch between `my-dark-theme' and `my-light-theme'"
  (interactive)
  (if (memq my-light-theme custom-enabled-themes)
      (progn
        (disable-theme my-light-theme)
        (load-theme my-dark-theme t)
        (if (fboundp '--extend-my-dark-theme)
            (--extend-my-dark-theme)))
    (progn
      (disable-theme my-dark-theme)
      (load-theme my-light-theme t)
      (if (fboundp '--extend-my-light-theme)
        (--extend-my-light-theme)))))

(global-set-key (kbd "<f5>") #'toggle-dark-light-theme)

(use-package base16-theme
  :ensure t
  :config
  (setq my-dark-theme 'base16-eighties
        my-light-theme 'base16-tomorrow))

(add-hook 'after-init-hook #'(lambda () (load-theme my-dark-theme t) (if (fboundp '--extend-my-dark-theme) (--extend-my-dark-theme))))

(provide 'init-ui)
