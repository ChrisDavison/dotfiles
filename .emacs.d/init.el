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

;; Languages
;; This section will load any packages and modify settings based on
;; the programming languages I use.  There isn't too much modification here.
;; Emacs provides a fair amount of support by default
(require 'davison-markdown)
(require 'davison-csv)
(require 'davison-css)
(require 'davison-haskell)
(require 'davison-javascript)

;; Python
;; C / C++
;; Go
;; Rust
;; Elisp
;; Lisp
;; Org  (? usability)
;; Version Control


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
 '(custom-safe-themes
   (quote
    ("573e46dadf8c2623256a164831cfe9e42d5c700baed1f8ecd1de0675072e23c2" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
