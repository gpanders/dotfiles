(defvar org-gtd-directory nil "Location of GTD org mode files.")

(setq org-gtd-directory
      (cond ((eq system-type 'darwin) "~/Documents/Notes/gtd/")
	    ((eq system-type 'gnu/linux) "~/notes/gtd/")))

(setq org-agenda-files `(,(expand-file-name "inbox.org" org-gtd-directory)
			 ,(expand-file-name "actions.org" org-gtd-directory)
			 ,(expand-file-name "tickler.org" org-gtd-directory)))
(setq org-capture-templates `(("t" "Todo [inbox]" entry
			       (file+headline ,(expand-file-name "inbox.org" org-gtd-directory) "Tasks")
			       "* TODO %i%?")
			      ("T" "Tickler" entry
			       (file+headline ,(expand-file-name "tickler.org" org-gtd-directory) "Tickler")
			       "* %i%? \n %U")))
(setq org-refile-targets `((,(expand-file-name "actions.org" org-gtd-directory) :maxlevel . 3)
			   (,(expand-file-name "someday.org" org-gtd-directory) :level . 1)
			   (,(expand-file-name "tickler.org" org-gtd-directory) :maxlevel . 2)))
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(provide 'init-org)
