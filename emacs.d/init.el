;; This sets up the load path so that we can override it
(package-initialize nil)

;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)

;; Try the 'use package' macro
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Finally, load my Org-mode (literate programming) config file
(org-babel-load-file "~/.emacs.d/Chris.org")
