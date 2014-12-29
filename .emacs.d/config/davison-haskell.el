(require-package 'haskell-mode)

(add-to-list 'completion-ignored-extensions ".hi")

;; Indentation
(require-package 'hi2)
(add-hook 'haskell-mode-hook 'turn-on-hi2)

(when (fboundp 'electric-indent-mode)
  (add-hook 'haskell-mode-hook (lambda () (electric-indent-mode -1))))

;; Hook auto-complete into the completions provided by the inferior
;; haskell process, if any.
(require-package 'ac-haskell-process)

(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c C-d") 'ac-haskell-process-popup-doc))

(after-load 'auto-complete
  (add-to-list 'ac-modes 'haskell-interactive-mode)
  (add-hook 'haskell-interactive-mode-hook 'set-auto-complete-as-completion-at-point-function))


(provide 'davison-haskell)
