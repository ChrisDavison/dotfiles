;;; ../code/dotfiles/.doom.d/autoload/orgutil.el -*- lexical-binding: t; -*-

;;; general org settings
(setq org-directory "~/Dropbox/org"
      org-default-notes-file "~/Dropbox/org/inbox.org"
      org-src-window-setup 'current-window
      org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-inhibit-startup nil
      org-indent-indentation-per-level 1
      org-adapt-indentation nil
      org-tags-column 0
      org-pretty-entities t
      org-catch-invisible-edits 'show-and-error
      org-imenu-depth 4
      org-link-frame-setup '((file . find-file-other-window))
      ;;       ;; Use M-+ M-- to change todo, and leave S-<arrow> for windows
      ;;       org-replace-disputed-keys t
      org-hide-emphasis-markers t
      org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "|" "DONE(d)" "CANCELLED(c)"))
      org-cycle-separator-lines 0
      org-list-indent-offset 2
      org-modules '(org-habit)
      ;;       org-modules '(org-bibtex org-habit org-tempo)
      org-log-repeat t
      org-log-done 'time
      org-log-done-with-time t
      org-treat-insert-todo-heading-as-state-change t
      org-log-into-drawer t
      org-archive-location "~/Dropbox/org/projects/done.org::* From %s"
      org-refile-use-outline-path 't
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
      org-roam-directory org-directory
      org-startup-folded 'fold
      org-id-track-globally t
        ;;; org-roam / deft / zetteldeft
      deft-directory org-directory
      deft-recursive t)

;;; org roam config

(setq org-roam-tag-separator " ")

(add-hook! org-mode
           'visual-line-mode
           'org-indent-mode
           'abbrev-mode
           'undo-tree-mode
           '(lambda () (org-superstar-mode 1))
           '(lambda () (set-face-italic 'italic t)))

;;; org capture

(setq org-capture-templates
      (doct
       `(("Todo"
          :keys "t"
          :file "projects/todo.org"
          :template "* TODO %?")

         ("Work"
          :keys "w"
          :template "* TODO %?"
          :children (("General" :keys "w" :file "projects/work.org" :headline "Tasks")
                     ("GlasData" :keys "g" :file "projects/glasdata.org")
                     ("Cybele" :keys "c" :file "projects/cybele.org")))

         ("JOURNAL"
          :keys "j"
          :file "journal.org"
          :children (("Logbook Item" :keys "j" :type item :template "- %?")
                     ("Logbook Entry" :keys "J" :template "* %?" :datetree t)))

         ;; ("LOGBOOK"
         ;;  :keys "l"
         ;;  :file "projects/work.org"
         ;;  :children (("Logbook Item" :keys "l" :type item :template "- %?")
         ;;             ("Logbook Entry" :keys "L" :template "* %?" :datetree t)))

         ("GAMING"
          :keys "g"
          :headline "Games to Buy"
          :template "* TODO %?"
          :children (("PC" :keys "p" :file "pc-gaming.org")
                     ("Nintendo Switch" :keys "n" :file "nintendo-switch.org")
                     ("Tabletop" :keys "t" :file "tabletop-games.org" :headline "UNFILED")))

         ("MEDIA"
          :keys "m"
          :type entry
          :template "* TODO %?"
          :children (("Watch" :keys "w" :file "projects/stuff-to-watch.org")
                     ("Music" :keys "m" :file "projects/music.org")))

         ("Guitar song to learn"
          :keys "G"
          :file "projects/guitar.org" :headline "Songs to Learn"
          :template "* TODO %?")

         ("Books / reading"
          :keys "b"
          :file "projects/reading.org" :headline "REFILE"
          :template "* TODO %?")

         ("Anki"
          :keys "a"
          :file "projects/todo.org" :headline "anki"
          :template "* TODO %?")
         )))

;; Make capture windows take of 90% of the frame
(set-popup-rule! "^CAPTURE" :side 'bottom :size 0.90 :select t :ttl nil)

;;; org-capture for literature
(defun read-capitalized-title ()
  (s-titleize (read-string "Title: ")))

(defun read-author ()
  (let ((name (read-string "Author: " "" nil nil)))
    (if (s-equals? name "")
        nil
      (format-author-name name))))

(defun format-author-name (author)
  (concat (seq-mapcat
           (lambda (author-part)
             (if (> (length author-part) 1)
                 (s-concat " " (s-capitalize author-part))
               (s-concat (s-capitalize author-part) ".")))
           (s-split " " author))))

(defun maybe-get-bibtex ()
  "Maybe get a DOI number for a reference"
  (let ((doi (read-string "DOI: " "" nil nil)))
    (if (s-equals? doi "")
        nil
      (s-concat ("\n")))))

(defun read-authors ()
  (setq authors (read-author)
        running t)
  (while running
    (setq input (read-author))
    (if (s-equals? input nil)
        (setq running nil)
      (setq authors (concat authors " and " input))))
  authors)

(setq capture-template-literature
      `("L" ,(emoji-heading 'all-the-icons-faicon "graduation-cap" "Literature")
        entry (file+headline "literature.org" "REFILE")
        "* TODO %(read-capitalized-title)\n\nAuthors: %(read-authors)\n\n#+BEGIN_SRC bibtex\n#+END_SRC"))



;;; org agenda
(setq org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-compact-blocks t
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-sort-notime-is-late nil
      org-agenda-remove-tags t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-files '("~/Dropbox/org/projects"
                         "~/Dropbox/org/journal.org"
                         "~/Dropbox/org/archive.org")
      org-agenda-time-grid '((daily today require-timed remove-match)
                             (900 1000 1100 1200 1300 1400 1500 1600 1700)
                             "......"
                             "")
      org-agenda-skip-archived-trees nil
      org-agenda-use-time-grid nil
      org-agenda-custom-commands
      '(("c" . "+my stuff")
        ("c1" "One day" ((agenda ""
                                 ((org-agenda-span 'day)
                                  (org-agenda-start-day "-0d")))))
        ("cw" "Work" ((todo ""
                            ((org-agenda-files
                              '("~/Dropbox/org/projects/work.org"
                                "~/Dropbox/org/projects/cybele.org"
                                "~/Dropbox/org/projects/iof2020.org"))
                             (org-agenda-overriding-header "Work")))))

        ("cT" "Todos, no books"
         ((todo "" ((org-agenda-tag-filter-preset
                     '("-readinglist" "-hobby"))))))

        ("R" . "+review")
        ("Rw" "Weekly Review (last 7 days' DONE)"
         ((agenda "" ((org-super-agenda-groups nil)
                      (org-agenda-span 7)
                      (org-agenda-start-day "-8d")
                      (org-agenda-entry-types '(:timestamp))
                      (org-agenda-show-log t)))))
        ("Rd" "DONE today"
         ((agenda "" ((org-agenda-span 1)
                      (org-agenda-start-day "-0d")
                      (org-agenda-entry-types '(:timestamp))
                      (org-agenda-show-log t)))))

        ("r" . "+reading")
        ("rr" "Reading - in progress" ((todo "WIP"
                            ((org-agenda-files '("~/Dropbox/org/projects/reading.org"))
                             (org-agenda-overriding-header "Books in Progress")))))
        ("rf" "Reading - future priorities"
         ((todo "TODO|DONE"
                ((org-agenda-files '("~/Dropbox/org/projects/reading.org"))
                 (org-agenda-overriding-header "Possible next books")
                 (org-agenda-regexp-filter-preset '("+\#[A-Za-z]"))))))
        )
      org-agenda-sorting-strategy
      '(
        (agenda habit-down time-up todo-state-up priority-down)
        (todo todo-state-down priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)
        )
      )

;; org-agenda super groups
;; Each group has an implicit boolean OR operator between its selectors.
;;
;; After the last group, the agenda will display items that didn't
;; match any of these groups, with the default order position of 99
(setq org-super-agenda-groups
      `(
        (:name "Habit" :habit t :order 1)
        (:name "DONE" :todo "DONE" :order 99)
        (:name "Today" :time-grid nil :order 2
         :todo "TODAY" :and (:deadline past :scheduled past))
        (:name "Work" :file-path "work.org" :file-path "cybele.org" :file-path "glasdata.org" :order 3)
        (:name "Todo" :and (:todo "TODO" :file-path "todo.org") :order 4)
        (:name "Projects" :todo "TODO" :order 5)
        (:discard :anything)
        ))

(defun org-agenda-books-in-progress ()
  (interactive)
  (let* (
         (org-super-agenda-groups
          `((:name "Books to Read" :file-path "projects/reading.org")
           (:discard (:anything t)))))
    (org-todo-list "WIP")))

(defun org-agenda-todo-list--work ()
  (interactive)
  (let* (
         (org-super-agenda-groups
          `((:name "Work" :file-path "work.org" :file-path "cybele.org" :file-path "glasdata.org")
           (:discard (:anything t)))))
    (org-todo-list)))

(defun org-agenda-todo-list--books ()
  (interactive)
  (let* ((org-super-agenda-groups
          `((:name "Todo" :discard (:file-path "projects/reading.org")))))
    (org-todo-list)))

(org-super-agenda-mode 1)


(setq org-journal-file-type 'yearly
      org-journal-file-format "logbook-%Y.org"
      org-journal-date-format "%F %A"
      org-journal-time-format ""
      org-journal-dir (f-join org-directory "projects"))
