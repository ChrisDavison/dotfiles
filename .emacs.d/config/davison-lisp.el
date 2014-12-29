;; Evaluate commands and replace expression with answer
(require-package 'lively)

(setq-default initial-scratch-message
              (concat ";; Happy hacking, " (or user-login-name "") "!\n\n"))

;; Automatic byte compilation
(require-package 'auto-compile)
(auto-compile-on-save-mode 1)
(auto-compile-on-load-mode 1)

;; Load .el if newer than corresponding .elc
(setq load-prefer-newer t)

;; Useful settings for any lime session
(defun davison/useful-lisp ()
  (rainbow-delimiters-mode t)
  (enable-paredit-mode)
  (when (fboundp 'aggressive-indent-mode)
    (aggressive-indent-mode)))

(add-hook 'lisp-mode 'davison/useful-lisp)
(add-hook 'emacs-lisp-mode 'davison/useful-lisp)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require-package 'slime)
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(provide 'davison-lisp)
