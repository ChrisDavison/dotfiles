;;; init --- My emacs configuration

;;; Commentary:
;;;
;;; Was originally an org-babel file, that was tangled, but deciding
;;; to try a multi-file approach for easier debugging, and potentially
;;; faster startup

;;; Code:

(setq
 user-full-name "Chris Davison"
 user-mail-address "c.jr.davison@gmail.com"
 user-emacs-directory "~/.emacs.d")

;; This sets up the load path so that we can override it
(package-initialize nil)

;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)

;; Add melpa as a package source
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Use-package is fantastic for concisely installing and configuring packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(add-to-list 'load-path user-emacs-directory)

(require 'cd-init)
(require 'cd-header) ;; Config for auto-file-headers
(require 'cd-appearance)
(require 'cd-history-and-sessions)
(require 'cd-editing)
(require 'cd-languages)
(require 'cd-navigation)
(require 'cd-utility)
(require 'cd-osx)
(require 'cd-newstuff)
(require 'cd-org)
(require 'cd-org-capture-templates)

(load-theme 'gruvbox)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(package-selected-packages
   (quote
    (yasnippet-snippets julia-repl julia-mode org-bullets org-bullets-mode uniquify ace-isearch auctex rust-mode haml-mode markdown-mode s ivy fold-dwim smartparens undo-tree virtualenvwrapper use-package tao-theme switch-window solarized-theme smartscan seoul256-theme scss-mode sass-mode rainbow-mode pandoc-mode nodejs-repl magit lua-mode key-chord json-mode js2-mode js-comint ibuffer-vc htmlize guide-key go-mode fullframe fold-dwim-org flymake-rust flycheck-rust flycheck-clojure f expand-region exec-path-from-shell evil-surround evil-smartparens evil-leader emmet-mode elpy diminish csv-mode counsel color-theme-sanityinc-tomorrow coffee-mode cargo anzu aggressive-indent ace-jump-mode)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
