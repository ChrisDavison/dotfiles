(require-package 'matlab-mode)

(autoload 'matlab-mode
  "matlab"
  "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)

(provide 'davison-matlab)
