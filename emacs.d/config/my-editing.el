;;; my-editing.el --- Text editing

;;; Commentary:

;;; Code:

;; parentheses

;; (use-package rainbow-delimiters
;;   :ensure t
;;   :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(show-paren-mode 1)
(use-package smartparens :ensure t
  :config 
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  (key-chord-define-global ">)" 'sp-forward-slurp-sexp)
  (key-chord-define-global ">(" 'sp-forward-barf-sexp)
  (key-chord-define-global "<)" 'sp-backward-slurp-sexp)
  (key-chord-define-global "<(" 'sp-backward-barf-sexp))

(use-package evil-smartparens :ensure t
  :diminish evil-smartparens-mode
  :config
  (add-hook 'smartparens-mode #'evil-smartparens)
  (add-hook 'smartparens-strict-mode #'evil-smartparens))

;; clean up spaces
(global-set-key (kbd "C-SPC") 'cycle-spacing)

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
(key-chord-define-global "tw" 'trunc-wrap)
(setq truncate-lines t)
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

(provide 'my-editing)
;;; my-editing.el ends here
