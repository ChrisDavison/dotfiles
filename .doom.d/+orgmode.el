;; ../code/dotfiles/.doom.d/autoload/orgutil.el -*- lexical-binding: t; -*-

;;; ==========================
;;; GENERAL ORG SETTINGS
;;; ==========================

(setq org-directory (expand-file-name "~/Dropbox/org")
      org-default-notes-file (expand-file-name "~/Dropbox/org/inbox.org")
      org-src-window-setup 'current-window
      org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-files '("~/Dropbox/org")
      org-agenda-inhibit-startup nil
      org-indent-indentation-per-level 1
      org-adapt-indentation nil
      org-tags-column 0
      org-pretty-entities t
      org-catch-invisible-edits 'show-and-error
      org-imenu-depth 4
      org-link-frame-setup '((file . find-file-other-window))
      org-hide-emphasis-markers t
      org-todo-keywords '((sequence "TODO(t)" "MAYB(m)" "NEXT(n)" "WAIT(W)" "BLCK(b)" "WIP(w)" "|" "DONE(d)" "KILL(k)"))
      org-cycle-separator-lines 0
      org-list-indent-offset 2
      org-modules '(org-habit)
      org-log-repeat nil
      org-log-done nil
      org-log-done-with-time nil
      org-treat-insert-todo-heading-as-state-change t
      org-log-into-drawer t
      org-archive-location "~/Dropbox/org/done.org::* From %s"
      org-refile-use-outline-path 't
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets `((org-agenda-files . (:maxlevel . 3)))
      org-startup-folded 'fold
      org-id-track-globally t)


(setq org-agenda-files
      (directory-files org-directory t ".*\.org"))

(add-hook! org-mode
           'visual-line-mode
           'org-indent-mode
           'abbrev-mode
           'mixed-pitch-mode
           'undo-tree-mode
           '(lambda () (set-face-italic 'italic t)))

;;; ==========================
;;; ORG-CAPTURE
;;; ==========================
(setq org-capture-templates
      (doct
       `(("Todo"
          :keys "t"
          :template "* TODO %?"
          :file "work.org"
          :children (("Todo (personal)" :keys "t" :file "todo.org")
                     ("Work - General" :keys "w" :headline "Tasks")
                     ("Work - GlasData" :keys "g" :headline "Tasks - IoF + GlasData")
                     ("Work Pitstop" :keys "p" :headline "Tasks - IoF + Pitstop")
                     ("Work Cybele" :keys "c" :headline "Tasks - Cybele")))

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
          :children (("Watch" :keys "w" :file "stuff-to-watch.org")
                     ("Music" :keys "m" :file "music.org")))

         ("Guitar song to learn"
          :keys "G"
          :file "guitar.org" :headline "Songs to Learn"
          :template "* TODO %?")

         ("Books / reading"
          :keys "b"
          :file "reading.org" :headline "REFILE"
          :template "* TODO %?")

         ("Anki"
          :keys "a"
          :file "todo.org" :headline "anki"
          :template "* TODO %?")

        ("Learn"
          :keys "l"
          :file "todo.org" :headline "Stuff to learn (refile to appropriate file)"
          :template "* TODO learn about: %?")
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

(setq auto-save-hook 'org-save-all-org-buffers)

;;; ==========================
;;; AGENDA
;;; ==========================
(setq org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-scheduled-if-done t
      org-agenda-block-separator ""
      org-agenda-skip-deadline-if-done t
      org-agenda-compact-blocks nil
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-sort-notime-is-late nil
      org-agenda-remove-tags nil
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-files '("~/Dropbox/org")
      org-agenda-time-grid '((daily today require-timed remove-match)
                             (900 1000 1100 1200 1300 1400 1500 1600 1700)
                             "......"
                             "")
      org-agenda-skip-archived-trees nil
      org-image-actual-width 600
      org-agenda-use-time-grid nil
      org-agenda-scheduled-leaders '("[S] " "[S %dx] ")
      org-agenda-deadline-leaders '("[D]" "[in %d days]")
      org-overriding-columns-format "%TODO %3PRIORITY %DEADLINE %40ITEM %TAGS"
      org-agenda-sorting-strategy
      '((agenda habit-up category-up scheduled-up  time-up todo-state-up  priority-down)
        (todo category-up todo-state-up priority-down)
        (tags priority-down category-keep)
        (search category-keep)))

(setq org-agenda-custom-commands
      '(("c" . "Custom agenda views")

        ("cc" "'Clean' - today's agenda only"

         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-day "-0d")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("BLCK" "WAIT")))))))
         ;; today's agenda, with overdue
         ;; HIDE blocked or stuff I've put on hold (BLCK WAIT)
         ;; show a todo list of IN-PROGRESS
         ;; show a todo list of BLOCKED or WAITING
         ;; show a todo list of POSSIBLE-NEXT-ITEMS
        ("c1" "One day"

         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-day "-0d")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("BLCK" "WAIT")))))
          (todo "WIP" ((org-agenda-overriding-header "In Progress")
                       (org-agenda-todo-ignore-scheduled t)))
          (todo "WAIT|BLCK" ((org-agenda-overriding-header "Blocked or on-hold")
                             (org-agenda-todo-ignore-scheduled t)))))

        ("cn" "NEXT" ((todo "NEXT" nil)))

        ("cw" "Work" ((todo ""
                            ((org-agenda-files '("~/Dropbox/org/work.org"))
                             (org-agenda-overriding-header "Work")))))

        ("ct" "Todos, no books"
         ((todo "" ((org-agenda-tag-filter-preset '("-readinglist"))))))

        ("cr" "Review the last week"
         ((agenda "" ((org-super-agenda-groups nil)
                      (org-agenda-span 7)
                      (org-agenda-start-day "-8d")
                      (org-agenda-entry-types '(:timestamp))
                      (org-agenda-show-log t)))))

        ("cp" "Planning"
         ((todo "WIP" ((org-agenda-overriding-header "IN PROGRESS")
                       (org-agenda-tag-filter-preset '("-readinglist"))))
          (todo "NEXT" ((org-agenda-overriding-header "POSSIBLE NEXT TASKS")
                        (org-agenda-tag-filter-preset '("-readinglist"))))
          (todo "TODO" ((org-agenda-overriding-header "TODO")
                        (org-agenda-tag-filter-preset '("-readinglist"))))
          (todo "MAYB" ((org-agenda-overriding-header "UNSURE ABOUT")
                        (org-agenda-tag-filter-preset '("-readinglist"))))))

        ("cR" "Reading (books in progress, and next options)"
         ((todo "WIP"
                ((org-agenda-files '("~/Dropbox/org/reading.org"))
                 (org-agenda-overriding-header "Books in Progress")))
          (todo "NEXT"
                ((org-agenda-files '("~/Dropbox/org/reading.org"))
                 (org-agenda-overriding-header "Possible next books")))
          (todo "MAYB"
                ((org-agenda-files '("~/Dropbox/org/reading.org"))
                 (org-agenda-overriding-header "Books to consider")))
          (todo "TODO"
                ((org-agenda-files '("~/Dropbox/org/reading.org"))
                 (org-agenda-overriding-header "Rest of the books...")))))

        ("cL" "Literature in progress, and next options"
         ((todo "WIP"
                ((org-agenda-files '("~/Dropbox/org/literature.org"))
                 (org-agenda-overriding-header "Papers in Progress")))
          (todo "NEXT"
                ((org-agenda-files '("~/Dropbox/org/literature.org"))
                 (org-agenda-overriding-header "Possible next papers")))))))

;;; ==========================
;;; JOURNAL
;;; ==========================
(after! f
  (setq org-journal-file-type 'yearly
      org-journal-file-format "logbook-%Y.org"
      org-journal-date-format "%F %A"
      org-journal-time-format ""
      org-journal-dir org-directory))


(dir-locals-set-class-variables
 'my-org-dir-local-stuff
 '((nil . ((projectile-project-root "~/Dropbox/org")))))
(dir-locals-set-directory-class "~/Dropbox/org" 'my-org-dir-local-stuff)
