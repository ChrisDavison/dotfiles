;;; ../code/dotfiles/.doom.d/+golang.el -*- lexical-binding: t; -*-

;;; Programming - golang
(add-to-list 'exec-path (concat (file-name-as-directory (getenv "GOPATH")) "bin") t)
(add-to-list 'load-path (concat (file-name-as-directory (getenv "GOPATH")) "src/github.com/dougm/goflymake"))
(require 'go-flymake)
                                        ; Use goimports instead of go-fmt for formatting with intelligent package addition/removal
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (go-eldoc-setup)
                                        ; call Gofmt before saving
                          (add-hook 'before-save-hook 'gofmt-before-save)))
