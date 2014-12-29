;;; package --- Sumary

;;; Commentary:
;; Bootstrap my configuration
;; Attempting my own clean and thin emacs install

;;; Code:

;; Add our directory of configuration files to the loadpath
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Some useful consts
(defconst *spell-check-support-enabled* nil) ;; Don't enable spellcheck
(defconst *is-a-mac* (eq system-type 'darwin))

;; Start loading, beginning with my utility macros
;; Settings for the package manager
;; And diminish, to reduce the name of the mode in the infobar
(require 'davison-utils)
(require 'davison-elpa)
(require-package 'diminish)

;; Appearance
(require 'davison-appearance) ;Modify appearance/theme

;; Usability
(require 'davison-osx-keys) ;Make some keys work on OSX
(require 'davison-dired) ;Built in neotree kinda thing
(require 'davison-isearch) ;Improve Emacs' search
(require 'davison-grep)
(require 'davison-ibuffer) ;Improve interactive buffer
(require 'davison-ido)
(require 'davison-windows) ;Managing windows/splits
(require 'davison-sessions) ;Session handling
(require 'davison-fonts) ;Font handling / resizing
(require 'davison-multiplemajor)
(require 'davison-editing) ;Various modifications to ease general editing
(require 'davison-org)
(require 'davison-keychord)
(require 'davison-vim)
(require 'davison-autocomplete)

;; Languages
;; This section will load any packages and modify settings based on
;; the programming languages I use.  There isn't too much modification here.
;; Emacs provides a fair amount of support by default
(require 'davison-flycheck)
(require 'davison-markdown)
(require 'davison-csv)
(require 'davison-css)
(require 'davison-haskell)
(require 'davison-javascript)
(require 'davison-lisp)
(require 'davison-matlab)
(require 'davison-go)
(require 'davison-rust)
(require 'davison-racket)


;; Python
;; C / C++
;; Go
;; Rust
;; Elisp
;; Lisp
;; Org  (? usability)

;; Require random packages
(require-package 'gnuplot)
(require-package 'htmlize)
(require-package 'regex-tool)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-monokai)))
 '(custom-safe-themes
   (quote
    ("41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" default)))
 '(racket-program "/usr/local/bin/racket")
 '(raco-program "/usr/local/bin/raco"))

