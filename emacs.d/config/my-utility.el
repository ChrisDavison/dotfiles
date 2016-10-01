;;; my-utility.el --- General utilities

;;; Commentary:

;;; Code:
(use-package s :ensure t)
(use-package f :ensure t)

;; help - guide-key
;; It's hard to remember keyboard shortcuts. The =guide-key= package
;; pops up help after a short delay.

(use-package guide-key :ensure t
  :diminish guide-key-mode
  :init (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  :config (guide-key-mode 1))

;; utf-8
;; From http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(provide 'my-utility)
;;; my-utility.el ends here
