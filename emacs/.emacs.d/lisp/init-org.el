(defvar org-gtd-directory nil "Location of GTD org mode files.")
(defvar org-wiki-directory nil "Location of wiki files.")
(defvar org-wiki-html-directory nil "Location of published wiki HTML files.")

(setq org-gtd-directory
      (cond ((eq system-type 'darwin) "~/Documents/ownCloud/GTD/")
            ((eq system-type 'gnu/linux) "~/Documents/GTD/")))

(setq org-wiki-directory
      (cond ((eq system-type 'darwin) "~/Documents/ownCloud/Notes/")
            ((eq system-type 'gnu/linux) "~/Documents/Notes/")))

(setq org-wiki-html-directory
      (cond ((eq system-type 'darwin) "~/Public/html/wiki")))

(defun open-gtd-inbox ()
  "Open GTD inbox."
  (interactive)
  (find-file (expand-file-name "inbox.org" org-gtd-directory)))

(defun open-gtd-actions ()
  "Open GTD next actions list."
  (interactive)
  (find-file (expand-file-name "actions.org" org-gtd-directory)))

(defun open-gtd-waiting ()
  "Open GTD waiting list."
  (interactive)
  (find-file (expand-file-name "waiting.org" org-gtd-directory)))

(defun open-gtd-projects ()
  "Open GTD projects list."
  (interactive)
  (find-file (expand-file-name "projects.org" org-gtd-directory)))

(defun open-gtd-tickler ()
  "Open GTD tickler."
  (interactive)
  (find-file (expand-file-name "tickler.org" org-gtd-directory)))

(defun open-gtd-someday-maybe ()
  "Open GTD someday/maybe list."
  (interactive)
  (find-file (expand-file-name "someday.org" org-gtd-directory)))

(defun open-wiki-index ()
  "Open wiki index."
  (interactive)
  (find-file (expand-file-name "index.org" org-wiki-directory)))

(setq org-publish-project-alist
      `(("wiki"
        :base-directory ,org-wiki-directory
        :publishing-directory ,org-wiki-html-directory
        :recursive t
        :publishing-function org-html-publish-to-html)))

(setq org-agenda-files `(,(expand-file-name "inbox.org" org-gtd-directory)
                         ,(expand-file-name "projects.org" org-gtd-directory)
                         ,(expand-file-name "actions.org" org-gtd-directory)
                         ,(expand-file-name "waiting.org" org-gtd-directory)
                         ,(expand-file-name "tickler.org" org-gtd-directory)))

(setq org-capture-templates `(("i" "Inbox" entry
                               (file+headline ,(expand-file-name "inbox.org" org-gtd-directory) "Inbox")
                               "* %i%?")
                              ("a" "Action" entry
                               (file+headline ,(expand-file-name "actions.org" org-gtd-directory) "Next Actions")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline ,(expand-file-name "tickler.org" org-gtd-directory) "Tickler")
                               "* %i%?\n %U")
                              ("d" "Diary" entry
                               (file ,(expand-file-name (concat "diary/" (format-time-string "%Y-%m-%d") ".org") (file-name-directory (directory-file-name org-gtd-directory))))
                               "* %i%?")))

(setq org-refile-targets `((,(expand-file-name "projects.org" org-gtd-directory) :maxlevel . 2)
                           (,(expand-file-name "actions.org" org-gtd-directory) :maxlevel . 1)
                           (,(expand-file-name "waiting.org" org-gtd-directory) :maxlevel . 1)
                           (,(expand-file-name "someday.org" org-gtd-directory) :level . 1)
                           (,(expand-file-name "tickler.org" org-gtd-directory) :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-export-with-section-numbers nil)
(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)
(setq org-html-head
      "<style type=\"text/css\">
  html { font-family: sans-serif; }
  body { padding: 2% 5%; margin: auto; }
  p, ul, ol, dl { line-height: 1.5em; }
  @media screen and (min-width: 48em) {
    body { max-width: 60%; }
  }
</style>")

(org-babel-do-load-languages 'org-babel-load-languages
                             '((python . t)))

(provide 'init-org)
