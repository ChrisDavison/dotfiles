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

(require 'my-progress-bar) ;; Show a progress bar during load, to debug slow config
(/// "init") (require 'my-init)
(/// "header") (require 'my-header) ;; Config for auto-file-headers
(/// "appearance") (require 'my-appearance)
(/// "vim") (require 'my-vim)
(/// "history") (require 'my-history-and-sessions)
(/// "editing") (require 'my-editing)
(/// "languages")(require 'my-languages)
(/// "navigation")(require 'my-navigation)
(/// "utility")(require 'my-utility)
(/// "osx")(require 'my-osx)
(/// "new stuff") (require 'my-new-stuff)
;; (/// "org")(require 'my-org)

(provide 'init)

;;; init ends here
