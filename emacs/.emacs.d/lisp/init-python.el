;;; init-python.el

(defun pyenv-executable-find (command)
  "Find executable taking pyenv shims into account."
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command))))
          (unless (string-match "not found" pyenv-string)
            pyenv-string)))
    (executable-find command)))

(defun python-setup-shell (&rest args)
  "Setup interactive Python shell."
  (when (pyenv-executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "--simple-prompt --no-color-info -i"
            python-shell-prompt-regexp "In \\[[0-9]+\\]: "
            python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
            python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
            python-shell-completion-setup-code
            "from IPython.core.completerlib import module_completion"
            python-shell-completion-string-code
            "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(defun inferior-python-setup ()
    "Configure inferior Python mode."
  (setq-local indent-tabs-mode t)
  (when (featurep 'evil)
    (evil-define-key 'insert inferior-python-mode-map (kbd "C-p") 'comint-previous-input)
    (evil-define-key 'insert inferior-python-mode-map (kbd "C-n") 'comint-next-input)))

(defun run-ipython ()
  "Start an interactive IPython shell."
  (interactive)
  (if (pyenv-executable-find "ipython")
      (progn
        (require 'term)
        (let* ((cmd "ipython")
               (args "-i")
               (switches (split-string-and-unquote args))
               (termbuf (apply 'make-term "IPython" cmd nil switches)))
          (set-buffer termbuf)
          (term-mode)
          (term-char-mode)
          (switch-to-buffer termbuf)))
    (message "IPython not found")))

(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.pyx\\'" . python-mode))
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map
              ("C-c C-y" . run-ipython))
  :init
  (python-setup-shell)
  (add-hook 'inferior-python-mode-hook #'inferior-python-setup))

(use-package pyenv-mode
  :ensure t
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions pyenv-mode-version)
  :init
  (setq pyenv-mode-mode-line-format "")

  (defun pyenv-add-venv-to-modeline ()
    "Add version string to the major mode name in the modeline."
    (let ((venv (pyenv-mode-version)))
      (setq mode-name
            (if venv
                (format "Python (%s)" venv)
              "Python"))))
  (add-hook 'python-mode-hook #'pyenv-add-venv-to-modeline)

  ;; Cribbed from Spacemacs
  (defun pyenv-mode-set-local-version ()
    "Set pyenv version from \".python-version\" by looking in parent directories."
    (interactive)
    (let ((root-path (locate-dominating-file default-directory
                                             ".python-version")))
      (when root-path
        (let* ((file-path (expand-file-name ".python-version" root-path))
               (version
                (with-temp-buffer
                  (insert-file-contents-literally file-path)
                  (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position)))))
          (if (member version (pyenv-mode-versions))
              (pyenv-mode-set version)
            (message "pyenv: version `%s' is not installed (set by %s)"
                     version file-path))))))
  (add-hook 'python-mode-hook #'pyenv-mode-set-local-version)
  (add-hook 'projectile-after-switch-project-hook #'pyenv-mode-set-local-version)

  (dolist (func '(pyenv-mode-set pyenv-mode-unset))
    (advice-add func :after 'python-setup-shell)))

;; (use-package elpy
;;   :disabled
;;   :ensure t
;;   :config
;;   (elpy-enable)
;;   (setq python-shell-interpreter "jupyter"
;;         python-shell-interpreter-args "console --simple-prompt"
;;         python-shell-prompt-detect-failure-warning nil)
;;   (add-to-list 'python-shell-completion-native-disabled-interpreters
;;                "jupyter"))

(use-package anaconda-mode
  :ensure t
  :after python
  :hook python-mode
  :init
  (setq anaconda-mode-eldoc-as-single-line t)
  :config
  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode)
  (with-eval-after-load 'evil
    (evil-define-key 'normal anaconda-mode-map (kbd "g r") 'anaconda-mode-find-references))
  (use-package company-anaconda
    :ensure t
    :after company
    :init
    (add-to-list 'company-backends 'company-anaconda)))

(use-package pip-requirements
  :ensure t
  :mode ("/requirements.txt$" . pip-requirements-mode))

(use-package ein
  :ensure t)

(provide 'init-python)

;;; init-python.el ends here
