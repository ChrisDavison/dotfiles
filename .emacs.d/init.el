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

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rust-mode haml-mode markdown-mode s ivy fold-dwim smartparens undo-tree virtualenvwrapper use-package tao-theme switch-window solarized-theme smartscan seoul256-theme scss-mode sass-mode rainbow-mode pandoc-mode nodejs-repl molokai-theme magit lua-mode key-chord json-mode js2-mode js-comint ibuffer-vc htmlize guide-key go-mode fullframe fold-dwim-org flymake-rust flycheck-rust flycheck-clojure f expand-region exec-path-from-shell evil-surround evil-smartparens evil-leader emmet-mode elpy diminish csv-mode counsel color-theme-sanityinc-tomorrow coffee-mode cargo avy anzu aggressive-indent ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
