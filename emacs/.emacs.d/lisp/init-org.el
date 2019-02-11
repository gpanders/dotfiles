(setq org-agenda-files `(,(expand-file-name "inbox.org" org-gtd-directory)
                         ,(expand-file-name "projects.org" org-gtd-directory)
                         ,(expand-file-name "actions.org" org-gtd-directory)
                         ,(expand-file-name "tickler.org" org-gtd-directory)))
(setq org-capture-templates `(("i" "Inbox" entry
                               (file+headline ,(expand-file-name "inbox.org" org-gtd-directory) "Inbox")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline ,(expand-file-name "tickler.org" org-gtd-directory) "Tickler")
                               "* %i%?\n %U")
                              ("d" "Diary" entry
                               (file ,(expand-file-name (concat "diary/" (format-time-string "%Y-%m-%d") ".org") (file-name-directory (directory-file-name org-gtd-directory))))
                               "* %i%?")))
(setq org-refile-targets `((,(expand-file-name "projects.org" org-gtd-directory) :maxlevel . 2)
                           (,(expand-file-name "actions.org" org-gtd-directory) :maxlevel . 1)
                           (,(expand-file-name "someday.org" org-gtd-directory) :level . 1)
                           (,(expand-file-name "tickler.org" org-gtd-directory) :maxlevel . 2)))
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((python . t)))

(provide 'init-org)
