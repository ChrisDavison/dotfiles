;;; ../code/dotfiles/.doom.d/autoload/orgutil.el -*- lexical-binding: t; -*-

;;; general org settings
(setq cd/todo-keywords '((sequence "TODO(t)" "WIP(w)" "|"
                                    "DONE(d)" "CANCELLED(c)")
                                        ; local use only (orgzly wont use)
                          (sequence "BACKBURNER(b)" "|" "FINISHED(f)")))
(setq org-directory "~/Dropbox/org"
      org-default-notes-file "~/Dropbox/org/inbox.org"
      org-src-window-setup 'current-window
      org-indent-indentation-per-level 1
      org-adapt-indentation nil
      org-tags-column -80
      org-pretty-entities t
      org-catch-invisible-edits 'show-and-error
      org-imenu-depth 4
      org-link-frame-setup '((file . find-file-other-window))
      ;;       ;; Use M-+ M-- to change todo, and leave S-<arrow> for windows
      ;;       org-replace-disputed-keys t
      org-hide-emphasis-markers t
      org-todo-keywords cd/todo-keywords
      org-cycle-separator-lines 0
      org-list-indent-offset 2
      org-modules '(org-habit)
      ;;       org-modules '(org-bibtex org-habit org-tempo)
      org-log-repeat t
      org-log-done 'time
      org-log-done-with-time t
      org-treat-insert-todo-heading-as-state-change t
      org-log-into-drawer t
      org-archive-location "~/Dropbox/org/archive.org::* From %s"
      org-refile-use-outline-path 'full-file-path
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
      org-roam-directory org-directory
      org-startup-folded 'fold
      org-id-track-globally t
        ;;; org-roam / deft / zetteldeft
      deft-directory org-directory
      deft-recursive t)

;;; capture templates
(setq cd/org-capture-templates
      `(

        ("t" ,(emoji-heading 'all-the-icons-octicon "checklist" "Todo")
         entry (file "projects/todo.org") "* TODO %?")
        ;; ("T" ,(emoji-heading 'all-the-icons-octicon "checklist" "Todo (Work)")
        ;;  entry (file+headline "projects/work.org" "Tasks - General") "* TODO %?")
       
        ("j" ,(emoji-heading 'all-the-icons-octicon "comment" "Journal (item)")
         item (file+olp+datetree "journal.org"))
        ("J" ,(emoji-heading 'all-the-icons-octicon "comment" "Journal (entry)")
         entry (file+olp+datetree "journal.org") "* %?" :empty-lines 1)



        ("l" ,(emoji-heading 'all-the-icons-octicon "repo" "Logbook (item)")
         item (file+olp+datetree "projects/work.org"))
        ("L" ,(emoji-heading 'all-the-icons-octicon "repo" "Logbook (entry)")
         entry (file+olp+datetree "projects/work.org"))

        ("g" ,(emoji-heading 'all-the-icons-faicon "gamepad" "Gaming"))
        ("gp" "PC" entry (file "pc-gaming.org") "* TODO %^{PC Game}")
        ("gn" "Nintendo Switch" entry (file "nintendo-switch.org") "* TODO %^{Nintendo Switch Game}")
        ("gt" "Tabletop" entry (file "tabletop-games.org") "* TODO %^{Tabletop game}")

        ("w" ,(emoji-heading 'all-the-icons-faicon "television" "Watch")
         entry (file+headline "projects/media.org" "UNFILED")
         "* TODO watch %?")

        ("m" ,(emoji-heading 'all-the-icons-faicon "headphones" "Music")
         entry (file+headline "projects/media.org" "Music")
         "* TODO %^{Music}" :immediate-finish t)

        ("G" ,(emoji-heading 'all-the-icons-faicon "music" "Guitar song to learn")
         entry (file+headline "projects/Guitar.org" "Songs to Learn")
         "* TODO %^{Guitar song}" :immediate-finish t)

        ("b" ,(emoji-heading 'all-the-icons-octicon "book" "Books / reading")
         entry (file+headline "projects/reading.org"  "REFILE")
         "* TODO %^{Book}" :immediate-finish t)

        ("a" ,(emoji-heading 'all-the-icons-octicon "comment" "Anki")
         entry (file+headline "projects/todo.org" "anki")
         "* TODO %^{Anki}" :immediate-finish t)

        ;; ("L" ,(emoji-heading 'all-the-icons-faicon "graduation-cap" "Literature")
        ;;  entry (file+headline "literature.org" "REFILE")
        ;;  "* TODO %(read-capitalized-title)\n\nAuthors: %(read-authors)\n\n#+BEGIN_SRC bibtex\n#+END_SRC"
        ;;  :immediate-finish t)
        ))

(setq org-capture-templates cd/org-capture-templates)

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
      org-agenda-use-time-grid nil
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

;;; org roam config
(defun tagify (str)
  (interactive "M")
  (s-join " " (--map (format "@%s" it) (s-split " " str))))

(defun roam-tagify (str)
  (interactive "Mtags: ")
  (evil-open-below 1)
  (insert (format "#+ROAM_TAGS: %s\n\n" str))
  (insert (tagify str))
  (evil-force-normal-state)
  (save-buffer))

(setq org-roam-tag-separator " ")

;;; other utility
(require 'org-id)
(defun cd/org-id-every-heading ()
  (interactive)
  (goto-char (point-max))
  (while (outline-previous-heading)
    (org-id-get-create)))

(add-hook! org-mode
           'visual-line-mode
           'org-indent-mode
           'abbrev-mode
           'undo-tree-mode
           '(lambda () (org-superstar-mode 1))
           '(lambda () (interactive) (setq org-capture-templates cd/org-capture-templates))
           '(lambda () (set-face-italic 'italic t)))



;;; agenda super groups
;; Each group has an implicit boolean OR operator between its selectors.
;;
;; After the last group, the agenda will display items that didn't
;; match any of these groups, with the default order position of 99
(setq org-super-agenda-groups
      `(
        (:name "Habit" :habit t)
        (:name "Today" :time-grid t
         :todo "TODAY" :and (:discard (:todo "CANCELLED") :deadline past :scheduled past))
        (:name "Important" :priority "A")
        (:name "In Progress" :todo "WIP")
        (:auto-category t)
        (:name "Todo" :todo "TODO" )
        (:name "Waiting" :todo "WAITING" :todo "WAIT" :tag "waiting")
        (:name "DONE" :todo "DONE" :date today :log closed :order 99)
        (:discard :anything)
        ))

(defun cd/agenda-books-in-progress ()
  (interactive)
  (let* ((bookpath (expand-file-name "~/Dropbox/org/projects/reading.org"))
         (org-super-agenda-groups
          `((:name "Books to Read" :file-path ,bookpath)
           (:discard (:anything t)))))
    (org-todo-list "WIP")))

(defun cd/agenda-todo-no-books ()
  (interactive)
  (let* ((bookpath (expand-file-name "~/Dropbox/org/projects/reading.org"))
         (org-super-agenda-groups
          `((:name "Todo" (:discard (:file-path bookpath))))))
    (org-todo-list)))


(org-super-agenda-mode 1)

;; Make capture windows take of 90% of the frame
(set-popup-rule! "^CAPTURE" :side 'bottom :size 0.90 :select t :ttl nil)

