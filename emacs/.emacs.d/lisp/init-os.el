(when (eq system-type 'darwin)
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
  (setq mac-option-modifier 'super)
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

(provide 'init-os)
