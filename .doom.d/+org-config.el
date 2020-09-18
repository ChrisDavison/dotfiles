;;; ../code/dotfiles/.doom.d/autoload/orgutil.el -*- lexical-binding: t; -*-
(map! :map org-mode-map
      :n "C-x C-n" 'cd/org-file-from-subtree
      :v "C-x C-n" 'cd/org-file-from-selection)

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

(setq cd/my-agenda-files
      '("~/Dropbox/org/projects" "~/Dropbox/org/journal"))
(defvar cd/archive-in-agenda t)
(when cd/archive-in-agenda
  (add-to-list 'cd/my-agenda-files "~/Dropbox/org/archive/archive.org")
  (setq org-agenda-files cd/my-agenda-files))
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
                                    "WIP(w)" ; prioritised task
                                    "|"
                                    "DONE(d)"
                                    "CANCELLED(c)"
                                    )
                          ; these are only for local usage. don't bother with them in orgzly.
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
      org-archive-location "~/Dropbox/org/archive/archive.org::"
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
      org-agenda-remove-tags t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-files cd/my-agenda-files
      org-agenda-time-grid '((daily today require-timed)
                             (900 1200 1300 1700)
                             "......"
                             "")
      org-id-track-globally t
        ;;; org-roam / deft / zetteldeft
      deft-directory org-directory
      deft-recursive t
      org-fancy-priorities-list '((?A . "H") (?B . "M") (?C . "L"))
      )
(after! org
  (add-to-list 'org-modules 'org-habit))
(map! :map org-mode-map
      :leader "N" 'org-toggle-narrow-to-subtree)

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

(require 'org-id)
(defun cd/org-id-every-heading ()
  (interactive)
  (goto-char (point-max))
  (while (outline-previous-heading)
    (org-id-get-create)))

(setq org-roam-tag-separator " ")

(add-hook! 'org-mode
           'visual-line-mode
           'org-indent-mode
           'abbrev-mode
           '(lambda () (set-face-italic 'italic t)))
