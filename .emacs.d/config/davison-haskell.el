(require-package 'haskell-mode)

;; Indentation
(require-package 'hi2)
(add-hook 'haskell-mode-hook 'turn-on-hi2)

(when (fboundp 'electric-indent-mode)
  (add-hook 'haskell-mode-hook (lambda () (electric-indent-mode -1))))

(provide 'davison-haskell)
