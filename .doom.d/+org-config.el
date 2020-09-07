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
      org-todo-keywords '((sequence "TODO(t)" ; just a task
                                    "ACTIVE(a)"  ; in progress
                                    "WAITING(w)"     ; my choice to put on hold
                                    "BLOCKED(b)"  ; waiting for someone
                                    "|"
                                    "DONE(d)"
                                    "CANCELED(c)"
                                    )
                          (sequence "PROJECT(p)" ; general project marker
                                    "FUTURE(f)"  ; on-hold project
                                    "|"
                                    "CANCELED(C)"))

      org-cycle-separator-lines 0
      org-list-indent-offset 2
      org-modules '(org-habit)
      ;;       org-modules '(org-bibtex org-habit org-tempo)
      org-log-repeat nil
      org-log-done 'time
      org-log-done-with-time t
      org-archive-location "~/Dropbox/org/archive.org::"
      org-refile-use-outline-path t
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
      org-roam-directory org-directory
      org-startup-folded 'fold
        ;;; agenda
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-compact-blocks t
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-sort-notime-is-late nil
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-files `("~/Dropbox/org/projects" "~/Dropbox/org/journal.org")
      org-id-track-globally nil
        ;;; org-roam / deft / zetteldeft
      deft-directory org-directory
      deft-recursive t
      )
