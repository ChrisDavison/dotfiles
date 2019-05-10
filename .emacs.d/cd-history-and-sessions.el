;;; cd-history-and-sessions.el --- Configuring history, sessions, backup, and undo

;;; Commentary:
;;; history, backups, session and undo Disk space is plentiful.  Keep
;;; backups and history.  Also, move the backups to the appropriate
;;; dir,so the backup files =.*~= don't clutter.

;;; Code:
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Counting 'recent files' as part of history
(use-package recentf :ensure t)
(setq recentf-max-saved-items 200)
(setq recentf-max-menu-items 15)
(recentf-mode)

;; Always save the desktop, the current workspace config
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)

;; Reload when file changed on disk
(global-auto-revert-mode t)

;; undo tree  - visualize your undos and branches

;; People often struggle with the Emacs undo model, where there's
;; really no concept of "redo" - you simply undo the undo.
;;
;; This lets you use =C-x u= (=undo-tree-visualize=) to visually walk
;; through the changes you've made, undo back to a certain point (or
;; redo), and go down different branches.
(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps nil)
  (setq undo-tree-visualizer-diff t))

(provide 'cd-history-and-sessions)
;;; cd-history-and-sessions.el ends here
