;;; cd-org.el --- Org-mode

;;; Commentary:
;;;
;;; Powerful note taking with org-mode

;;; Code:

;; taking notes
;; Set up org for taking notes, using Dropbox/docs as my main
;; folder. Also, set up some nice config for org todo and agenda
;; stuff. =org-refile= lets you organize notes by typing in the
;; headline to file them under.

(global-set-key (kbd "C-c q") 'auto-fill-mode)

(use-package org
  :ensure t
  :bind (("<f1>" . org-capture)
         ("<f2>" . org-agenda)
         ("<f3>" . org-agenda-list)
         ("C-c l" . org-store-link))
  :config
  (setq org-directory "~/Dropbox/notes"
        org-default-notes-file "~/Dropbox/inbox.org"
        org-src-window-setup 'current-window
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-todo-keywords '((sequence "TODO" "WIP" "|" "DONE" "CANCELLED"))
        org-startup-indented t
        org-hide-leading-stars t
        org-agenda-files '("~/Dropbox/" "~/Dropbox/projects" "~/Dropbox/archive")
        org-log-done 'time)
  ;; Settings for refiling
  (setq org-reverse-note-order t
        org-refile-use-outline-path nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-cache nil
        org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
        org-blank-before-new-entry nil)
  ;; (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (setq fill-column 80))
(use-package org-bullets :ensure t)

;; (use-package ox-reveal :ensure t)
(use-package htmlize :ensure t)

;; This makes it easier to add links from outside.
(defun sacha/yank-more ()
  "Yank into an org link."
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'sacha/yank-more)

;; indent org babel src
;; In an Org-Babel block, run my/org-cleanup to fix indentation
(defun cd/org-cleanup ()
  (interactive)
  (org-edit-special)
  (indent-buffer)
  (org-edit-src-exit))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-x c") 'cd/org-cleanup)

(setq org-imenu-depth 3)

(if (eq system-type 'windows-nt)
    (setq inhibit-compacting-font-caches t))


;; babel
(setq org-babel-load-languages
      '((emacs-lisp . t)
        (R . t)
        (Python . t)))

(setq org-confirm-babel-evaluate nil)

(define-skeleton org-skeleton
  "Header info for a emacs-org file."
  "-----\n"
  "#+TITLE: " (skeleton-read "Title: ") "\n"
  "#+AUTHOR: Chris Davison\n"
  "#+EMAIL: c.jr.davison@gmail.com\n"
  "#+OPTIONS: toc:2 num:nil html-postamble:nil\n"
  "#+PROPERTY: header-args :tangle " (skeleton-read "Tangle filename: ") "\n")
;;(global-set-key [C-S-f4] 'org-skeleton)

;; latex exporting

(require 'ox-latex)

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

;; This function makes checkbox counting work with HEADER checkboxes, as well as sublists.
;; (defun wicked/org-update-checkbox-count (&optional all)
;;   "Update the checkbox statistics in the current section.
;; This will find all statistic cookies like [57%] and [6/12] and update
;; them with the current numbers.  With optional prefix argument ALL,
;; do this for the whole buffer."
;;   (interactive "P")
;;   (save-excursion
;;     (let* ((buffer-invisibility-spec (org-inhibit-invisibility)) 
;;            (beg (condition-case nil
;;                     (progn (outline-back-to-heading) (point))
;;                   (error (point-min))))
;;            (end (move-marker
;;                  (make-marker)
;;                  (progn (or (outline-get-next-sibling) ;; (1)
;;                             (goto-char (point-max)))
;;                         (point))))   
;;            (re "\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)")
;;            (re-box
;;             "^[ \t]*\\(*+\\|[-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
;;            b1 e1 f1 c-on c-off lim (cstat 0))
;;       (when all
;;         (goto-char (point-min))
;;         (or (outline-get-next-sibling) (goto-char (point-max))) ;; (2)
;;         (setq beg (point) end (point-max)))
;;       (goto-char beg)
;;       (while (re-search-forward re end t)
;;         (setq cstat (1+ cstat)
;;               b1 (match-beginning 0)
;;               e1 (match-end 0)
;;               f1 (match-beginning 1)
;;               lim (cond
;;                    ((org-on-heading-p)
;;                     (or (outline-get-next-sibling) ;; (3)
;;                         (goto-char (point-max)))
;;                     (point))
;;                    ((org-at-item-p) (org-end-of-item) (point))
;;                    (t nil))
;;               c-on 0 c-off 0)
;;         (goto-char e1)
;;         (when lim
;;           (while (re-search-forward re-box lim t)
;;             (if (member (match-string 2) '("[ ]" "[-]"))
;;                 (setq c-off (1+ c-off))
;;               (setq c-on (1+ c-on))))
;;           (goto-char b1)
;;           (insert (if f1
;;                       (format "[%d%%]" (/ (* 100 c-on)
;;                                           (max 1 (+ c-on c-off))))
;;                     (format "[%d/%d]" c-on (+ c-on c-off))))
;;           (and (looking-at "\\[.*?\\]")
;;                (replace-match ""))))
;;       (when (interactive-p)
;;         (message "Checkbox statistics updated %s (%d places)"
;;                  (if all "in entire file" "in current outline entry")
;;                  cstat)))))
;; (defadvice org-update-checkbox-count (around wicked activate)
;;   "Fix the built-in checkbox count to understand headlines."
;;   (setq ad-return-value
;;         (wicked/org-update-checkbox-count (ad-get-arg 1))))

(provide 'cd-org)
;;; cd-org.el ends here
