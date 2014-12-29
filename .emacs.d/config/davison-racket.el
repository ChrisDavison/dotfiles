(require-package 'racket-mode)

;; (add-hook 'racket-mode-hook
;;           '(lambda ()
;;            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

(add-hook 'racket-mode-hook 'paredit-mode)
(add-hook 'racket-mode-hook 'rainbow-delimiters-mode)

(put 'test-case 'racket-indent-function 1)

(provide 'davison-racket)
