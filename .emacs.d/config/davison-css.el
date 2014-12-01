;;; SASS and SCSS
(require-package 'sass-mode)
(require-package 'scss-mode)
(setq-default scss-compile-at-save t)

;;; Colourise CSS colour literals
(when (maybe-require-package 'rainbow-mode)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))

(provide 'davison-css)
