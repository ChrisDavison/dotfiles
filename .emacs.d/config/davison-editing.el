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

(global-set-key (kbd "M-s") 'ace-jump-mode)

(provide 'davison-editing)
