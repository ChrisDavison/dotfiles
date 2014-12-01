(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode))

;; Whitespace
(defun hide-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

;; Newlines
(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

;; Undo Tree
(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; Show matching parens
(show-paren-mode 1)

;; Multiple cursors
;; Not really used this yet, but found it in another's config
(require-package 'multiple-cursors)

(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; From active region to multiple cursors
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c c") 'mc/edit-lines)
(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)

;; Kill backwards to indentation
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


(provide 'davison-editing)
