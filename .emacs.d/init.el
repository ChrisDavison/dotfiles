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

(defvar config-dir
  (expand-file-name "config/" user-emacs-directory))
(add-to-list 'load-path config-dir)

(require 'my-init)
(require 'my-header) ;; Config for auto-file-headers
(require 'my-appearance)
(require 'my-history-and-sessions)
(require 'my-editing)
(require 'my-languages)
(require 'my-navigation)
(require 'my-utility)
(require 'my-osx)
(require 'my-newstuff)
(require 'my-org)
(require 'org-capture-templates)

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
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")
     ("???" . "#dc752f"))))
 '(package-selected-packages
   (quote
    (ace-isearch auctex rust-mode haml-mode markdown-mode s ivy fold-dwim smartparens undo-tree virtualenvwrapper use-package tao-theme switch-window solarized-theme smartscan seoul256-theme scss-mode sass-mode rainbow-mode pandoc-mode nodejs-repl magit lua-mode key-chord json-mode js2-mode js-comint ibuffer-vc htmlize guide-key go-mode fullframe fold-dwim-org flymake-rust flycheck-rust flycheck-clojure f expand-region exec-path-from-shell evil-surround evil-smartparens evil-leader emmet-mode elpy diminish csv-mode counsel color-theme-sanityinc-tomorrow coffee-mode cargo anzu aggressive-indent ace-jump-mode)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
