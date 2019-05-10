;;; cd-org-capture-templates.el --- 

;;; Commentary:
;; These are all the capture templates I use for org-mode

;;; Code:
(setq org-capture-templates
      '(("q" "Quotes" entry (file+headline "~/Dropbox/reference/quotes.org" "UNFILED")
         "* %^{Topic}\n#+BEGIN_QUOTE\n%^{Quote} (%^{Author})\n#+END_QUOTE" :immediate-finish 1)

        ("u" "URL" item (file+headline "~/Dropbox/inbox.org" "Links")
         "[[%^{URL}][%^{DESCRIPTION}]] %^{COMMENTS}\n" :immediate-finish 1)

        ("p" "Project" entry (file "~/Dropbox/projects.org") "* %^{PROJECT}")

        ("a" "Article" entry (file+headline "~/Dropbox/projects.org" "Literature")
         "* TODO %^{Title} %(org-set-tags) :article:
:PROPERTIES:
:CREATED: %U
:END:
%i
Abstract:
%?"
         :prepend t
         :created t)

        ;; Todos (tasks within inbox)
        ;; Header-bullet of -TODO- <TASK>, under the TASKS L1 header
        ("t" "Todo")
        ("tt" "Todo" entry (file+headline "~/Dropbox/inbox.org" "Tasks")
         "* TODO %^{TASK}" :immediate-finish 1)
        ("tT" "Todo with pasted clipboard" entry (file+headline "~/Dropbox/inbox.org" "Tasks")
         "* TODO %^{TASK}\n%c" :immediate-finish 1)

        ;; Journal
        ;; Datetree of YYYY / YYYY-MM MONTHNAME / YYYY-MM-DD DAYNAME
        ("j" "Journal entry")
        ("jj" "Quick entry" item (file+datetree "~/Dropbox/journal.org")
         "%^{Journal}" :immediate-finish 1)
        ("jJ" "Full entry" item (file+datetree "~/Dropbox/journal.org")
         "%^{Journal}")

        ;; LOGBOOK
        ("l" "Logbook entry")
        ("ll" "Quick entry" item (file+datetree "~/Dropbox/logbook.org")
         "%^{Logbook}" :immediate-finish 1)
        ("lL" "Full entry" item (file+datetree "~/Dropbox/logbook.org")
         "%^{Logbook}")

        ;; Note in Inbox
        ("n" "Note in Inbox")
        ("nn" "Note" item (file+headline "~/Dropbox/inbox.org" "Notes")
         "%^{NOTE}" :immediate-finish 1)
        ("nN" "Note with clipboard" item (file+headline "~/Dropbox/inbox.org" "Notes")
         "%^{NOTE} %c")

        ("c" "Code Snippet" entry (file+headline "~/Dropbox/inbox.org" "Code Snippets")
         "* %^{Snippet Topic}\n#+BEGIN_SRC %^{Language}\n%c\n#+END_SRC\n")
        ))

(provide 'cd-org-capture-templates)
;;; cd-org-capture-templates.el ends here
