;;; ../code/dotfiles/.doom.d/+org.el -*- lexical-binding: t; -*-

;;; ORG MODE
(after! org
  (setq org-directory "~/code/knowledge/"
        ;; Make 'd' the default priority, so priorities sort properly (ABC then D is blank)
        org-priority-default 68 ;; make 'D' the default priority, so ABC priorities sort
        org-priority-lowest 68 ;; make 'D' the lowest priority
        org-src-window-setup 'current-window
        org-indent-indentation-per-level 1
        org-adapt-indentation nil
        org-tags-column -120
        org-pretty-entities t
        org-catch-invisible-edits 'show-and-error
        org-imenu-depth 4
        org-link-frame-setup '((file . find-file-other-window))
        org-hide-emphasis-markers t
        org-todo-keywords '((sequence "TODO(t)" "MAYB(m)" "NEXT(n)" "WAIT(W)" "BLCK(b)" "WIP(w)" "|" "DONE(d)" "KILL(k)"))
        org-cycle-separator-lines 0
        org-list-indent-offset 2
        org-modules nil
        org-treat-insert-todo-heading-as-state-change t
        org-log-repeat nil
        org-log-done 'time
        org-log-done-with-time nil
        org-log-into-drawer t
        org-archive-location (format "%s::* From %%s" (f-join org-directory "archive.org"))
        org-refile-use-outline-path 't
        org-refile-allow-creating-parent-nodes 'confirm
        org-startup-folded 'fold
        org-id-track-globally t
        org-image-actual-width 600
        org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
;;; Org ROAM
        org-roam-directory org-directory
        deft-directory org-directory
        +org-roam-open-buffer-on-find-file nil
        org-roam-rename-file-on-title-change nil
        org-superstar-headline-bullets-list '("▶" "▷" "▸" "▹" "◆" "◇" "◈")
        ;; '("✠" "✙" "✚" "✜" "✛" "✢" "✣" "✤")
        ))

;;; Org CAPTURE
(after! org
  (load! "+literature_capture")
  (setq org-capture-templates
        (doct `(
                ("Todo" :keys "t"
                 :type entry
                 :template "* TODO %?"
                 :children (("Todo [PERSONAL]" :keys "t" :file "projects/todo.org")
                            ("Todo [WORK]" :keys "w" :file "projects/work.org")))

                ;; ("Todo [WORK]" :keys "w"
                ;;  :file "projects/work.org"
                ;;  :type entry
                ;;  :template "* TODO %?")

                ("Calendar" :keys "c"
                 :empty-lines 1
                 :datetree t
                 :type entry
                 :template "* TODO %?"
                 :children (("Calendar [PERSONAL]" :keys "c"
                             :file (lambda () (f-join org-directory (format-time-string "journal-%Y.org"))))
                            ("Calendar [WORK]" :keys "w"
                             :file (lambda () (f-join org-directory (format-time-string "logbook-%Y.org"))))))

                ;; ("Calendar [WORK]" :keys "a"
                ;;  :empty-lines 1
                ;;  :file (lambda () (f-join org-directory (format-time-string "journal-%Y.org")))
                ;;  :datetree t
                ;;  :type entry
                ;;  :template "* TODO %?")

                ;; ("New file" :keys "n"
                ;;  :file ,(lambda () (let* ((input (read-string "Title: " "" nil nil))
                ;;                      (filename (format "%s.org" (s-join "-" (s-split " " input)))))
                ;;                 (f-join org-directory filename)))
                ;;  :type plain)

                ;; ("Paste to todo" :keys "p"
                ;;  :file "projects/todo.org"
                ;;  :type entry
                ;;  :immediate-finish t
                ;;  :template "* TODO %^{TITLE}\n\n[[%c][%^{URL Title}]]\n")

                ("Journal" :keys "j"
                 :empty-lines 1
                 :file (lambda () (f-join org-directory (format-time-string "journal-%Y.org")))
                 :datetree t
                 :type entry
                 :template "* %?")

                ("Logbook" :keys "l"
                 :empty-lines 1
                 :file (lambda () (f-join org-directory (format-time-string "logbook-%Y.org")))
                 :datetree t
                 :type entry
                 :template "* %?")

                ("Literature" :keys "L"
                 :empty-lines 1
                 :file "projects/literature.org" :headline "REFILE"
                 :type entry
                 :immediate-finish t
                 :template "* TODO %(read-capitalized-title)\n\n%(read-authors)")

                ;; ("Guitar song to learn" :keys "g"
                ;;  :file "projects/guitar.org" :headline "Songs to Learn"
                ;;  :immediate-finish t
                ;;  :template "* TODO %^{Artist} - /%^{Title}/")

                ;; ("Books / reading" :keys "b"
                ;;  :file "reading/reading.org" :headline "REFILE"
                ;;  :type item
                ;;  :template "%^{Book name}")

                ;; ("Anki" :keys "a"
                ;;  :file "todo.org" :headline "anki"
                ;;  :immediate-finish t
                ;;  :template "* TODO %^{To Anki}")

                ;; ("Learn" :keys "l"
                ;;  :file "todo.org" :headline "Stuff to learn (refile to appropriate file)"
                ;;  :immediate-finish t
                ;;  :template "* TODO learn about: %^{learn}")

                ;;   ("GAMING" :keys "g"
                ;;    :headline "Games to Buy"
                ;;    :template "* TODO %^{Game}"
                ;;    :immediate-finish t
                ;;    :children (("PC" :keys "p" :file "pc-gaming.org")
                ;;               ("Nintendo Switch" :keys "n" :file "nintendo-switch.org")))

                ;; ("Current work PROJECT" :keys "p"
                ;;  :type item
                ;;  :file (lambda () (f-join "~/code/knowledge/tasks/" cd/current-work-project))
                ;;  :function find-todays-headline-or-create)

                ;; ("MEDIA" :keys "m"
                ;;  :type entry
                ;;  :children (("Watch" :keys "w" :file "projects/stuff-to-watch.org"
                ;;              :headline "REFILE" :template "* TODO %?")
                ;;             ("Music" :keys "m" :file "projects/music.org"
                ;;              :headline "REFILE"
                ;;              :template "* TODO /\"%^{Title}\",/ by %^{Artist}"
                ;;              :immediate-finish t)))
                )))
  )

;;; Org AGENDA
(after! org
  (setq org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-inhibit-startup nil
        org-agenda-files `(,(f-join org-directory "reading")
                           ,(f-join org-directory "projects")
                           ,(f-join org-directory (format-time-string "journal-%Y.org"))
                           ,(f-join org-directory (format-time-string "logbook-%Y.org")))
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
        org-agenda-use-time-grid t
        org-agenda-scheduled-leaders '("" "LATE x%d")
        org-agenda-deadline-leaders '("" " in %dd")
        org-overriding-columns-format "%TODO %3PRIORITY %DEADLINE %40ITEM %TAGS"
        org-agenda-sorting-strategy '((agenda habit-up time-up category-up scheduled-up todo-state-up  priority-down)
                                      (todo category-up todo-state-down priority-down)
                                      (tags priority-down category-keep)
                                      (search category-keep)))

  (setq cd/work-and-engd-files (append `(,(f-join org-directory (format-time-string "logbook-%Y.org")))
                                       (files-matching-tagsearch (f-join org-directory "projects") "work"))
        cd/engd-files (files-matching-tagsearch (f-join org-directory "projects") "engd")
        cd/work-files (cl-set-difference cd/work-and-engd-files cd/engd-files :test 'equal)
        cd/non-work-files (cl-set-difference (org-agenda-files) (append cd/work-files cd/engd-files)
                                             :test 'equal))

  (let ((cd/org-project (lambda (filename) (f-join org-directory "projects" filename))))
    (setq org-agenda-custom-commands
          `(("c" . "Custom agenda views")

            ("c1" "One day"
             ((agenda "" ((org-agenda-span 'day)
                          (org-agenda-use-time-grid t)
                          (org-agenda-start-day "-0d")))

              ;; show a todo list of IN-PROGRESS
              (todo "WIP" ((org-agenda-overriding-header "IN PROGRESS")
                           (org-agenda-todo-ignore-scheduled t)))

              (todo "WIP|NEXT|MAYB|TODO" ((org-agenda-overriding-header "Reading")
                                          (org-agenda-files `(,(f-join org-directory "reading" "books")))
                                          (org-agenda-todo-ignore-scheduled t)))

              (todo "WIP|NEXT|MAYB|TODO" ((org-agenda-overriding-header "UNFILED")
                                          (org-agenda-files `(,(f-join org-directory "projects/todo.org")
                                                              ,(f-join org-directory "projects/work.org")))
                                          (org-agenda-todo-ignore-scheduled t)))))

            ("cn" "NEXT" ((todo "NEXT" nil)))

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

            ("cw" "Work"
             ((todo "" ((org-agenda-files cd/work-files)
                        (org-agenda-overriding-header "Work")))
              (todo "" ((org-agenda-files cd/engd-files)
                        (org-agenda-overriding-header "EngD")))))

            ("ct" "Todos, no books"
             ((todo "" ((org-agenda-tag-filter-preset '("-readinglist"))))))

            ("cR" "Reading -- in progress, and possible future books"
             ((todo "WIP|NEXT|MAYB|TODO"
                    ((org-agenda-files '(,(f-join org-directory "reading")))
                     (org-agenda-overriding-header "Books in Progress")))))

            ("cL" "Literature in progress, and next options"
             ((todo "WIP|NEXT"
                    ((org-agenda-files '(,(f-join org-directory "literature.org")))
                     (org-agenda-overriding-header "Papers in Progress")))))

            ))))

;;; Org DOWNLOAD (+dragndrop)
(setq-default org-download-method 'directory)
(setq org-download-image-dir '(lambda () (interactive) (get-relative-asset-dir)))

;;; Org-mode hooks
(add-hook! org-mode
           'visual-line-mode
           '(lambda () (interactive) (setq fill-column 120))
           #'visual-fill-column-mode
           'org-indent-mode
           'abbrev-mode
           ;; 'mixed-pitch-mode
           'undo-tree-mode
           '(lambda () (set-face-italic 'italic t)))
(add-hook! 'auto-save-hook 'org-save-all-org-buffers)


;;; My Functions
(require 'org-id)
(defun org-id-every-heading ()
  (interactive)
  (goto-char (point-max))
  (while (outline-previous-heading)
    (org-id-get-create)))

(defun remove-org-mode-properties ()
  (interactive)
  (goto-char (point-min))
  (query-replace-regexp
   (rx bol (0+ space) ":" (0+ alnum) ":" (0+ anything) "\n")
   ""))

(defun find-next-md-org-pair (&optional backward)
  "Find the next file (by name) in the current directory.

With prefix arg, find the previous file."
  (interactive "P")
  (when buffer-file-name
    (let* ((file (if (s-equals? "md" (file-name-extension buffer-file-name))
                     buffer-file-name
                   (substring buffer-file-name 0 -4)))
           (files (files-in-curdir-with-ext "md"))
           (direction (if backward -1 1))
           (pos (mod (+ (cl-position file files :test 'equal) direction)
                     (length files))))
      (delete-other-windows)
      (find-file (nth pos files))
      (find-file-other-window (s-concat (buffer-file-name (find-file (nth pos files))) ".org")))))

(defun set-file-as-main-work-project ()
  (interactive)
  (setq cd/current-work-project (file-name-nondirectory (buffer-file-name))))

(setq org-babel-python-command "~/.envs/py/bin/python3")
(org-recent-headings-mode)


(add-to-list 'org-structure-template-alist '("p" . "src python"))
