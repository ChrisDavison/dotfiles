;;; cd-editing.el --- Text editing

;;; Commentary:

;;; Code:

;; parentheses

(show-paren-mode 1)
(use-package smartparens :ensure t
  :config 
  (require 'smartparens-config))

;; clean up spaces
(global-set-key (kbd "C-S-SPC") 'cycle-spacing)

;; expand region
(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

;; word wrapping and truncation
;; Couldn't get this working directly...so functionalise it
(defun trunc-wrap()
  "Turn on truncation and word wrapping"
  (interactive)
  (if truncate-lines 
      (progn
        (setq truncate-lines nil)
        (setq word-wrap t)
        (message "Truncation and word wrap enabled"))
    (progn 
      (setq truncate-lines t)
      (setq word-wrap nil)
      (message "Truncation and word wrap disabled"))))
(setq truncate-lines nil)
(setq word-wrap t)

;; aggressive indentation
(use-package aggressive-indent :ensure t
  :config (global-aggressive-indent-mode))

;; indent after newline
(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;; 'Zen-move' - distraction free editing
(use-package darkroom :ensure t)

(provide 'cd-editing)
;;; cd-editing.el ends here
