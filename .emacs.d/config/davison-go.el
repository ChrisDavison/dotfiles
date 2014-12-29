(require-package 'go-mode)
(require-package 'go-autocomplete)

(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'davison-go)
