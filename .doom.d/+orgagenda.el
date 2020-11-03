;;; ../code/dotfiles/.doom.d/+orgagenda.el -*- lexical-binding: t; -*-

;;; org agenda settings
(defvar cd/work-agenda-files '("~/Dropbox/org/projects/work.org"
                               "~/Dropbox/org/projects/cybele.org"
                               "~/Dropbox/org/projects/iof2020.org"))
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
      org-agenda-use-time-grid t
      org-agenda-custom-commands
      '(("c" . "+my stuff")
        ("c1" "One day" ((agenda ""
                                 ((org-agenda-span 'day)
                                  (org-todo-keywords cd/todo-keywords)
                                  (org-agenda-start-day "-0d")))))
        ("cw" "Work" ((todo ""
                            ((org-agenda-files cd/work-agenda-files)
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


;;; agenda super groups
;; Each group has an implicit boolean OR operator between its selectors.
;;
;; After the last group, the agenda will display items that didn't
;; match any of these groups, with the default order position of 99
(setq org-super-agenda-groups
      `(
        (:name "Habit" :habit t)
        (:name "Today" :time-grid t
         :todo "TODAY" :and (:deadline past :scheduled past))
        (:name "Work" :file-path "work.org" :file-path "cybele.org" :file-path "glasdata.org")
        (:name "Todo" :todo "TODO" )
        (:name "DONE" :not (:todo "TODO"))
        (:discard :anything)
        ))

(defun cd/agenda-books-in-progress ()
  (interactive)
  (let* (
         (org-super-agenda-groups
          `((:name "Books to Read" :file-path "projects/reading.org")
           (:discard (:anything t)))))
    (org-todo-list "WIP")))

(defun cd/agenda-todo-no-books ()
  (interactive)
  (let* ((org-super-agenda-groups
          `((:name "Todo" :discard (:file-path "projects/reading.org")))))
    (org-todo-list)))

(org-super-agenda-mode 1)
