(when (eq system-type 'darwin)
    ;; Set meta key to command when on Mac
    (setq mac-command-modifier 'meta
	  mac-option-modifier nil))

(provide 'init-os)
