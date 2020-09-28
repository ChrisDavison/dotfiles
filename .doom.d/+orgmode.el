;;; ../code/dotfiles/.doom.d/autoload/orgutil.el -*- lexical-binding: t; -*-

;;; general org settings
(setq org-directory "~/Dropbox/org"
      org-default-notes-file "~/Dropbox/org/inbox.org"
      org-src-window-setup 'current-window
      org-indent-indentation-per-level 1
      org-adapt-indentation nil
      org-pretty-entities t
      org-catch-invisible-edits 'show-and-error
      org-imenu-depth 4
      org-link-frame-setup '((file . find-file-other-window))
      ;;       ;; Use M-+ M-- to change todo, and leave S-<arrow> for windows
      ;;       org-replace-disputed-keys t
      org-hide-emphasis-markers t
      org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "|"
                                    "DONE(d)" "CANCELLED(c)")
                                        ; local use only (orgzly wont use)
                          (sequence "BACKBURNER(b)" "|" "FINISHED(f)"))

      org-cycle-separator-lines 0
      org-list-indent-offset 2
      org-modules '(org-habit)
      ;;       org-modules '(org-bibtex org-habit org-tempo)
      org-log-repeat t
      org-log-done 'time
      org-log-done-with-time t
      org-treat-insert-todo-heading-as-state-change t
      org-log-into-drawer t
      org-archive-location "~/Dropbox/org/archive.org::"
      org-refile-use-outline-path 'full-file-path
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
      org-roam-directory org-directory
      org-startup-folded 'fold
        ;;; agenda

      org-id-track-globally t
        ;;; org-roam / deft / zetteldeft
      deft-directory org-directory
      deft-recursive t
      org-fancy-priorities-list '((?A . "H") (?B . "M") (?C . "L"))
      )

(setq org-capture-templates
      (doct `(
              (,(emoji-heading 'all-the-icons-octicon "checklist" "Todo")
               :keys "t"
               :template "* TODO %?"
               :children (("Todo" :keys "t" :file "~/Dropbox/org/projects/todo.org")
                          ("Work" :keys "w" :file "~/Dropbox/org/projects/work.org" :header "Tasks")
                          ("IoF" :keys "i" :file "~/Dropbox/org/projects/iof2020.org" :header "Tasks")
                          ("Cybele" :keys "c" :file "~/Dropbox/org/projects/cybele.org" :header "Tasks")
                          ))

              (,(emoji-heading 'all-the-icons-octicon "repo" "Logbook")
               :keys "l"
               :file "projects/work.org"
               :datetree t
               :children (("Item" :keys "l" :type item :template "- %?")
                          ("Entry" :keys "L" :type entry :template "* %?")
                          ("Dated entry" :keys "d" :type entry :template "* %?"
                           :time-prompt t)
                          ("Cybele" :keys "c" :type item :template "- %?"
                           :file "projects/cybele.org")
                          ("GlasData" :keys "g" :type item :template "- %?"
                           :file "projects/glasdata.org")))

              (,(emoji-heading 'all-the-icons-faicon "gamepad" "Gaming")
               :keys "g"
               :template "* TODO %?"
               :headline "UNFILED"
               :children (("PC" :keys "p" :file "pc-gaming.org")
                          ("Nintendo Switch" :keys "n" :file "nintendo-switch.org")
                          ("Tabletop" :keys "t" :file "tabletop-games.org")))

              (,(emoji-heading 'all-the-icons-faicon "television" "Watch")
               :keys "w"
               :file "projects/media.org"
               :headline "UNFILED"
               :template "* TODO %?")

              (,(emoji-heading 'all-the-icons-faicon "headphones" "Music")
               :keys "m"
               :file "projects/media.org"
               :headline "Music"
               :template "* TODO %?")

              (,(emoji-heading 'all-the-icons-faicon "music" "Guitar")
               :keys "G"
               :file "projects/Guitar.org"
               :headline "Songs to Learn"
               :template "* TODO %?")

              (,(emoji-heading 'all-the-icons-octicon "book" "Book")
               :keys "b"
               :file "projects/reading.org"
               :headline "REFILE"
               :template "* TODO %?")

              (,(emoji-heading 'all-the-icons-octicon "comment" "Quote")
               :keys "Q"
               :file "journal.org"
               :type item
               :datetree t
               :template "- /\"%?\"/")

              (,(emoji-heading 'all-the-icons-octicon "comment" "Anki")
               :keys "a"
               :file "projects/todo.org"
               :headline "anki"
               :template "* TODO %?")

              (,(emoji-heading 'all-the-icons-faicon "graduation-cap" "Literature")
               :keys "L"
               :file "literature.org"
               :headline "REFILE"
               :template "* TODO %(read-capitalized-title)\n\nAuthors: %(read-authors)\n\n#+BEGIN_SRC bibtex\n#+END_SRC"
               :immediate-finish t))))

(setq cd/my-capture-templates org-capture-templates)

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
                         "~/Dropbox/org/journal"
                         "~/Dropbox/org/archive.org")
      org-agenda-time-grid '((daily today require-timed)
                             (900 1000 1100 1200 1300 1400 1500 1600 1700)
                             "......"
                             "")
      org-agenda-use-time-grid nil
      org-agenda-custom-commands
      '(("c" . "+my stuff")
        ("c1" "One day" ((agenda ""
                                 ((org-agenda-span 'day)
                                  (org-agenda-start-day "-0d")))))
        ("cw" "Work" ((todo ""
                            ((org-agenda-files cd/work-agenda-files)
                             (org-agenda-overriding-header "Work")))))
        ("cb" "Reading in progress" ((todo ""
                            ((org-agenda-files '("~/Dropbox/org/projects/reading.org"))
                             (org-agenda-overriding-header "Books in Progress")))))

        ("cW" "Weekly Review (last 7 days' DONE)"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-day "-7d")
                      (org-agenda-entry-types '(:timestamp))
                      (org-agenda-show-log t)))))
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

;;; org journal settings
(setq org-journal-file-format "%Y%m%d--%A.org"
      org-journal-find-file 'find-file
      org-journal-date-format "%A, %d %B %Y"
      org-journal-file-header "#+TITLE: %Y-%m-%d %A\n\n")


;;; other utility
(require 'org-id)
(defun cd/org-id-every-heading ()
  (interactive)
  (goto-char (point-max))
  (while (outline-previous-heading)
    (org-id-get-create)))

(add-hook! 'org-mode
           'visual-line-mode
           'org-indent-mode
           'abbrev-mode
           '(lambda () (set-face-italic 'italic t))
           '(lambda () (interactive) (org-superstar-mode -1)))
