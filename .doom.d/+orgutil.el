;;; ../code/dotfiles/.doom.d/autoload/orgutil.el -*- lexical-binding: t; -*-

(defun cd/refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))

(defun cd/refile-to-file (&optional target)
  (interactive)
  (let ((filename (or target (read-file-name "Refile to: ")))
        (old-refile-targets org-refile-targets))
    (progn (setq org-refile-targets `((filename . (:maxlevel . 6))))
           (org-refile)
           (setq org-refile-targets old-refile-targets))))

(defun cd/refile-to-this-file ()
  (interactive)
  (refile-to-file (buffer-name)))

(require 'org-element)

(defun org-file-from-subtree ()
  "Cut the subtree currently being edited and create a new file from it.

  If called with the universal argument, prompt for new filename,
  otherwise use the subtree title"
  (interactive "P")
  (let ((filename (expand-file-name (read-file-name "New file name:"))))
    (org-cut-subtree)
    (find-file-noselect filename)
    (with-temp-file filename
      (org-mode)
      (yank))
    (find-file filename)))
(define-key org-mode-map (kbd "C-x C-n") 'org-file-from-subtree)

(after! org-roam
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-journal" "j" #'org-journal-new-entry
        :desc "org-roam-capture" "c" #'org-roam-capture))

;; Much of the commented stuff is either stuff that I'm not sure about
;; or isn't actually different from the org-mode (or doom org) defaults.
(setq org-directory "~/Dropbox/org"
      org-default-notes-file "~/Dropbox/org/inbox.org"
      org-src-window-setup 'current-window
      org-indent-indentation-per-level 1
      org-adapt-indentation nil
      org-pretty-entities t
      org-catch-invisible-edits 'show-and-error
      org-imenu-depth 4
      ;;       ;; Use M-+ M-- to change todo, and leave S-<arrow> for windows
      ;;       org-replace-disputed-keys t
      org-hide-emphasis-markers t
      org-todo-keywords '((sequence "TODO(t)" "WIP" "WAIT" "|" "DONE")
                          (sequence "|" "DEAD"))
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-cycle-separator-lines 0
      org-list-indent-offset 2
      ;;       org-modules '(org-bibtex org-habit org-tempo)
      org-agenda-files '("~/Dropbox/org/projects" "~/Dropbox/org/inbox.org")
      org-log-repeat nil
      org-log-done t
      org-log-done-with-time t
      org-archive-location "~/Dropbox/org/archive.org::"
      org-refile-use-outline-path t
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-roam-directory org-directory
      org-startup-folded 'fold
      org-agenda-todo-ignore-scheduled 'future)
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "inbox.org" "UNFILED Tasks")
         "* TODO %?")
        ("r" "Research" entry (file "inbox.org")
         "** TODO Research %?")

        ("n" "Note")
        ("nn" "List item" item (file+headline "inbox.org" "Notes")
         "- %?")
        ("nl" "List link" item (file+headline "inbox.org" "Notes")
         "- [[%c][%^{Description}]] %?")
        ("nN" "Entry" entry (file+headline "inbox.org" "Notes")
         "* %?")

        ("l" "Logbook")
        ("ll" "Logbook item" item (file+datetree "logbook.org")
         "- %?")
        ("lL" "Logbook entry" entry (file+datetree "logbook.org")
         "* %?")
        ("ld" "Logbook entry (dated)" entry (file+datetree "logbook.org")
         "* %?" :time-prompt t)

        ("g" "Games")
        ("gp" "PC" entry
         (file+headline "pc-games.org" "UNFILED")
         "* TODO %^{PC game}")
        ("gn" "Nintendo Switch" entry
         (file+headline "nintendo-switch-games.org" "UNFILED")
         "* TODO %^{Nintendo Switch game}\n")
        ("gt" "Tabletop" entry
         (file+headline "tabletop-games.org" "UNFILED")
         "* TODO %^{Tabletop game}\n")

        ("w" "Watch" entry (file+headline "projects/media.org" "UNFILED")
         "* TODO %?")

        ("L" "Literature" entry (file+headline "literature.org" "REFILE")
         "** TODO %(read-capitalized-title)\n\nAuthors: %(read-authors)\n\n#+BEGIN_SRC bibtex\n#+END_SRC" :immediate-finish t)

        ("b" "book" entry (file+olp "projects/reading-list.org" "REFILE")
         "* %^{Book}\n%^{AUTHOR}p")

        ;; ("c" "Calendar" entry (file+olp+datetree "calendar.org")
        ;;  "* TODO %?\nDEADLINE: %t" :time-prompt t)

        ("j" "journal")
        ("jj" "Journal Item" item (file+datetree "journal.org") "%?")
        ("jJ" "Journal Entry" entry (file+datetree "journal.org") "* %?")

        ("Q" "Quote" entry (file "quotes.org")
         "* %^{Quote Topic}\n#+BEGIN_QUOTE\n%^{Quote} (%^{Author})\n#+END_QUOTE")
        ))
(org-roam-mode)

;; (require 'company-org-roam)
;; (use-package company-org-roam
;;   :when (featurep! :completion company)
;;   :after org-roam
;;   :config
;;   (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

;; (use-package! org-journal
;;       :custom
;;       (org-journal-dir "~/Dropbox/org/journal")
;;       (org-journal-date-prefix "#+TITLE: ")
;;       (org-journal-file-format "%Y-%m.org")
;;       (org-journal-date-format "%A, %d %B %Y"))
;; (setq org-journal-enable-agenda-integration t)

(defun my/org-skip-function (part)
  "Partitions things to decide if they should go into the agenda '(agenda future-scheduled done)"
  (let* ((skip (save-excursion (org-entry-end-position)))
         (dont-skip nil)
         (scheduled-time (org-get-scheduled-time (point)))
         (result
          (or (and scheduled-time
                   (time-less-p (time-add (current-time) (* 24 60 60)) scheduled-time)
                   'future-scheduled)  ; This is scheduled for a future date
              (and (org-entry-is-done-p) ; This entry is done and should probably be ignored
                   'done)
              'agenda)))                 ; Everything else should go in the agenda
    (if (eq result part) dont-skip skip)))
(setq org-agenda-skip-function '(my/org-skip-function 'agenda))

;;; org-roam / deft / zetteldeft
(setq deft-directory org-directory
      deft-recursive t)
;; (after! org-roam
;;         (map! :leader
;;             :prefix "n"
;;             :desc "org-roam" "l" #'org-roam
;;             :desc "org-roam-insert" "i" #'org-roam-insert
;;             :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
;;             :desc "org-roam-find-file" "f" #'org-roam-find-file
;;             :desc "org-roam-show-graph" "g" #'org-roam-show-graph
;;             :desc "org-roam-insert" "i" #'org-roam-insert
;;             :desc "org-journal" "j" #'org-journal-new-entry
;;             :desc "org-roam-capture" "c" #'org-roam-capture))
;; (add-hook! 'after-init-hook 'org-roam-mode)

;; (require 'company-org-roam)
;; (use-package company-org-roam
;;   :when (featurep! :completion company)
;;   :after org-roam
;;   :config
;;   (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
