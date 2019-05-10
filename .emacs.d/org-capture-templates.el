;;; cd-org-capture-templates.el --- 

;;; Commentary:
;; These are all the capture templates I use for org-mode

;;; Code:
(setq org-capture-templates
      '(("q" "quotes" entry (file "~/Dropbox/reference/quotes.org")
         "* %^{Topic}\n#+BEGIN_QUOTE\n%^{Quote} (%^{Author})\n#+END_QUOTE" :immediate-finish 1)

        ("u" "url" item (file+headline "~/Dropbox/inbox.org" "Links")
         "[[%^{URL}][%^{DESCRIPTION}]] %^{COMMENTS}\n" :immediate-finish 1)

        ;; Header-bullet of -TODO- <TASK>, under the TASKS L1 header
        ("t" "todo" entry (file+headline "~/Dropbox/inbox.org" "Tasks")
         "* TODO %^{TASK}" :immediate-finish 1)

        ("T" "todo with clipboard" entry (file+headline "~/Dropbox/inbox.org" "Tasks")
         "* TODO %^{TASK} %c" :immediate-finish 1)

        ("r" "reading" entry (file+headline "~/Dropbox/inbox.org" "Read")
         "* TODO Read %^{Read}")
        
        ("p" "project" entry (file "~/Dropbox/projects.org")
         "* %^{PROJECT}")

        ;; Datetree of YYYY / YYYY-MM MONTHNAME / YYYY-MM-DD DAYNAME
        ("j" "Journal note" item (file+datetree "~/Dropbox/journal.org")
         "%^{Journal}" :immediate-finish 1)

        ("J" "Journal note (with editing)" item (file+datetree "~/Dropbox/journal.org")
         "%^{Journal}")

        ("l" "Logbook note" item (file+datetree "~/Dropbox/logbook.org")
         "%^{Logbook}" :immediate-finish 1)

        ("L" "Logbook note (with editing)" item (file+datetree "~/Dropbox/logbook.org")
         "%^{Logbook}")
        
        ("n" "note" item (file+headline "~/Dropbox/inbox.org" "Notes")
         "%^{NOTE}" :immediate-finish 1)

        ("N" "note with clipboard" item (file+headline "~/Dropbox/inbox.org" "Notes")
         "%^{NOTE} %c")
        ))

(provide 'cd-org-capture-templates)
;;; cd-org-capture-templates.el ends here
