;;; ../code/dotfiles/.doom.d/autoload/orgutil.el -*- lexical-binding: t; -*-

;;; general org settings
(setq cd/todo-keywords '((sequence "TODO(t)" "WIP(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-directory "~/Dropbox/org"
      org-default-notes-file "~/Dropbox/org/inbox.org"
      org-src-window-setup 'reorganize-frame
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

(setq cd/first-org-reload-done nil)
(defun cd/reload-org-on-first-startup ()
  (cd/reload-org-config)
  (setq cd/first-org-reload-done t))

(add-hook! org-mode
           'visual-line-mode
           'org-indent-mode
           'abbrev-mode
           'undo-tree-mode
           '(lambda () (org-superstar-mode 1))
           '(lambda () (interactive) (setq org-capture-templates cd/org-capture-templates))
           'cd/reload-org-on-first-startup
           '(lambda () (set-face-italic 'italic t)))
