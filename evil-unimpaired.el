;;; evil-unimpaired.el --- Pairs of handy bracket mappings.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a port of vim-unimpaired https://github.com/tpope/vim-unimpaired
;; `evil-unimpaired' provides pairs of handy bracket mappings to quickly navigate
;; to previous/next thing and more.

;;; Code:

(require 'seq)
(require 'evil)

(defvar evil-unimpaired-leader-keys '("[" . "]")
  "The pair of leader keys used to execute the pair mappings.")

(defvar evil-unimpaired-default-pairs
  '(("SPC" (evil-unimpaired-insert-space-above . evil-unimpaired-insert-space-below))
    ("b" (previous-buffer . next-buffer))
    ("f" (evil-unimpaired-previous-file . evil-unimpaired-next-file))
    ("t" (evil-unimpaired-previous-frame . evil-unimpaired-next-frame))
    ("w" (previous-multiframe-window . next-multiframe-window))
    ("p" (evil-unimpaired-paste-above . evil-unimpaired-paste-below)))
  "binding pairs for evil normal state")

(defun evil-unimpaired--find-relative-filename (offset)
  (when buffer-file-name
    (let* ((directory (file-name-directory buffer-file-name))
	   (files (seq-filter 'file-regular-p
			      (directory-files directory
					       'full
					       (rx bos ;; ignore auto-save-files
						   (optional ".")
						   (not (any ".#"))))))
           (index (+ (seq-position files buffer-file-name) offset))
           (file (and (>= index 0) (nth index files))))
      (when file
        (expand-file-name file directory)))))

(defun evil-unimpaired-previous-file ()
  (interactive)
  (if-let (filename (evil-unimpaired--find-relative-filename -1))
      (find-file filename)
    (user-error "No previous file")))

(defun evil-unimpaired-next-file ()
  (interactive)
  (if-let (filename (evil-unimpaired--find-relative-filename 1))
      (find-file filename)
    (user-error "No next file")))

(defun evil-unimpaired-paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun evil-unimpaired-paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))

(defun evil-unimpaired-insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun evil-unimpaired-insert-space-below (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun evil-unimpaired-next-frame ()
  (interactive)
  (raise-frame (next-frame)))

(defun evil-unimpaired-previous-frame ()
  (interactive)
  (raise-frame (previous-frame)))

;;;###autoload
(define-minor-mode evil-unimpaired-mode
  "Global minor mode to provide convient pairs of bindings"
  :keymap (make-sparse-keymap)
  :global t
  (evil-normalize-keymaps))

(defun evil-unimpaired-define-pair (key funcs &optional state)
  "create an evil-unimpaired pair binding.
Bind KEY in STATE to PREV and NEXT. STATE can be an evil state or
a list of states and defaults to 'normal."
  (dolist (fetcher '(car cdr))
    (let ((evil-state (if state state 'normal))
	  (key-binding (kbd (concat (funcall fetcher evil-unimpaired-leader-keys) " " key)))
	  (func (funcall fetcher funcs)))
      (evil-define-key evil-state evil-unimpaired-mode-map key-binding func))))

(dolist (pair evil-unimpaired-default-pairs)
  (apply 'evil-unimpaired-define-pair pair))

(provide 'evil-unimpaired)
;;; evil-unimpaired.el ends here.
