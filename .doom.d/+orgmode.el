;; ../code/dotfiles/.doom.d/autoload/orgutil.el -*- lexical-binding: t; -*-

;;; ==========================
;;; GENERAL ORG SETTINGS
;;; ==========================
(setq org-directory (expand-file-name "~/code/knowledge/")
      org-src-window-setup 'current-window
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
      ;; org-modules '(org-habit)
      org-modules nil
      org-treat-insert-todo-heading-as-state-change t
      org-log-repeat nil
      org-log-done 'time
      org-log-done-with-time nil
      org-log-into-drawer t
      org-archive-location (format "%s::* From %%s" (concat org-directory "archive.org"))
      org-refile-use-outline-path 't
      org-refile-allow-creating-parent-nodes 'confirm
      org-startup-folded 'fold
      org-id-track-globally t
      org-image-actual-width 600

;;; ==========================
;;; AGENDA
;;; ==========================
      org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-inhibit-startup nil
      org-agenda-files `(,org-directory
                         ,(concat org-directory "books")
                         ,(concat org-directory "work"))
      org-refile-targets `((org-agenda-files . (:maxlevel . 3)))
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-skip-archived-trees nil
      org-agenda-block-separator ""
      org-agenda-compact-blocks nil
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-sort-notime-is-late nil
      org-agenda-remove-tags t
      org-agenda-time-grid '((daily today require-timed remove-match)
                             (900 1000 1100 1200 1300 1400 1500 1600 1700)
                             "......"
                             "")
      org-agenda-use-time-grid nil
      org-agenda-scheduled-leaders '("" "LATE x%d")
      org-agenda-deadline-leaders '("" " in %dd")
      org-overriding-columns-format "%TODO %3PRIORITY %DEADLINE %40ITEM %TAGS"
      org-agenda-sorting-strategy '((agenda habit-up time-up category-up scheduled-up todo-state-up  priority-down)
                                    (todo priority-down category-up todo-state-down )
                                    (tags priority-down category-keep)
                                    (search category-keep))

;;; ==========================
;;; JOURNAL
;;; ==========================
      org-journal-file-type 'yearly
      org-journal-file-format "logbook-%Y.org"
      org-journal-date-format "%F %A"
      org-journal-time-format ""
      org-journal-dir org-directory
;;; ==========================
;;; DEFT
;;; ==========================
      deft-directory org-directory
)


;;; ==========================
;;; CUSTOM CAPTURES
;;; ==========================
;;; also have +literature_capture.el, which is unused
(setq org-capture-templates
      (doct
       `(("Todo"
          :keys "t"
          :template "* TODO %?"
          :file "work.org"
          :children (("Todo @personal" :keys "t" :file "todo.org")
                     ("Work @general" :keys "w" :headline "Tasks")
                     ("Work @glasdata" :keys "g" :headline "Tasks - IoF + GlasData")
                     ("Work @pitstop" :keys "p" :headline "Tasks - IoF + Pitstop")
                     ("Work @cybele" :keys "c" :headline "Tasks - Cybele")))

         ("GAMING"
          :keys "g"
          :headline "Games to Buy"
          :template "* TODO %^{Game}"
          :immediate-finish t
          :children (("PC" :keys "p" :file "pc-gaming.org")
                     ("Nintendo Switch" :keys "n" :file "nintendo-switch.org")
                     ("Tabletop" :keys "t" :file "tabletop-games.org" :headline "UNFILED")))

         ("MEDIA"
          :keys "m"
          :type entry
          :children (("Watch" :keys "w" :file "stuff-to-watch.org" :template "* TODO %?")
                     ("Music" :keys "m" :file "music.org"
                      :template "* TODO /\"%^{Title}\",/ by %^{Artist}"
                      :immediate-finish t)))

         ("Guitar song to learn"
          :keys "G"
          :file "guitar.org" :headline "Songs to Learn"
          :immediate-finish t
          :template "* TODO /\"%^{Title}\",/ by %^{Artist}")

         ("Books / reading"
          :keys "b"
          :file "reading.org" :headline "REFILE"
          :immediate-finish t
          :template "* TODO %^{Read}")

         ("Anki"
          :keys "a"
          :file "todo.org" :headline "anki"
          :immediate-finish t
          :template "* TODO %^{To Anki}")

        ("Learn"
          :keys "l"
          :file "todo.org" :headline "Stuff to learn (refile to appropriate file)"
          :immediate-finish t
          :template "* TODO learn about: %^{learn}")
         )))

;;; ==========================
;;; CUSTOM AGENDAS
;;; ==========================
(defun org-files-work ()
  (--map (concat org-directory it)
         '("work.org" "literature.org")))

(defun org-files-non-work ()
  (cl-set-difference
   (org-agenda-files) (org-files-work)
   :test 'equal))

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
          (todo "WIP" ((org-agenda-overriding-header "Work - In Progress")
                       (org-agenda-files (org-files-work))
                       (org-agenda-todo-ignore-scheduled t)))
          (todo "WIP" ((org-agenda-overriding-header "In Progress")
                       (org-agenda-files (--filter (not (s-contains? "work.org" it)) (org-agenda-files)))
                       (org-agenda-todo-ignore-scheduled t)))
          (todo "WAIT|BLCK" ((org-agenda-overriding-header "Blocked or on-hold")
                             (org-agenda-todo-ignore-scheduled t)))))

        ("cn" "NEXT" ((todo "NEXT" nil)))

        ("cw" "Work" ((todo ""
                            ((org-agenda-files '((concat org-directory "work.org")))
                             (org-agenda-overriding-header "Work")))))

        ("ct" "Todos, no books"
         ((todo "" ((org-agenda-tag-filter-preset '("-readinglist"))))))

        ("cr" "Review the last week"
         ((agenda "" ((org-agenda-start-day "-8d")
                      (org-agenda-entry-types '(:timestamp))
                      (org-agenda-archives-mode t)
                      (org-agenda-later 1)
                      (org-agenda_log-mode 16)
                      (org-agenda-show-log t)))))

        ("cp" "Planning"
         ;; kept as multiple todo commands so that grouping is done by todo state
         ;; rather than by category (which is my default todo sorting preference)
         ((todo "WIP" ((org-agenda-overriding-header "IN PROGRESS")
                       (org-agenda-tag-filter-preset '("-readinglist"))))
          (todo "NEXT" ((org-agenda-overriding-header "POSSIBLE NEXT TASKS")
                        (org-agenda-tag-filter-preset '("-readinglist"))))
          (todo "TODO" ((org-agenda-overriding-header "TODO")
                        (org-agenda-tag-filter-preset '("-readinglist"))))
          (todo "MAYB" ((org-agenda-overriding-header "UNSURE ABOUT")
                        (org-agenda-tag-filter-preset '("-readinglist"))))))

        ("cR" "Reading -- in progress, and possible future books"
         ((todo "WIP|NEXT|MAYB|TODO"
                ((org-agenda-files '((concat org-directory "reading.org")))
                 (org-agenda-overriding-header "Books in Progress")))
          ))

        ("cL" "Literature in progress, and next options"
         ((todo "WIP|NEXT"
                ((org-agenda-files '((concat org-directory "literature.org")))
                 (org-agenda-overriding-header "Papers in Progress")))))

        ("ca" "Stuff to anki"
         ((todo "" ((org-agenda-regexp-filter-preset '("+anki"))
                    (org-agenda-sorting-strategy '((todo todo-state-down priority-down category-up))))))
         )))



(add-hook! org-mode
           'visual-line-mode
           'org-indent-mode
           'abbrev-mode
           'mixed-pitch-mode
           'undo-tree-mode
           '(lambda () (set-face-italic 'italic t)))

(add-hook! 'auto-save-hook 'org-save-all-org-buffers)
