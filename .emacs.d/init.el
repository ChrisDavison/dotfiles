;;; init --- My emacs configuration

;;; Commentary:

;;; Code:
(setq gc-cons-threshold 100000000)

;; Utility functions to check if my org file has been tangled recently
(defun file-mod-time (filename)
  (file-attribute-modification-time (file-attributes filename)))

(defconst cd/config-org "~/code/dotfiles/.emacs.d/chris-davison.org" "Path to my configuration")
(defconst cd/config-el "~/code/dotfiles/.emacs.d/chris-davison.el" "Output filename for my configuration")

(require 'org)
(org-babel-tangle-file cd/config-org cd/config-el)
(load cd/config-el t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (doom-modeline doom-themes delight diminish zenburn-theme yasnippet-snippets visual-fill-column virtualenvwrapper use-package undo-tree switch-window solarized-theme smartscan smartparens seoul256-theme scss-mode sass-mode rainbow-mode rainbow-delimiters racer pandoc-mode org-bullets nodejs-repl molokai-theme material-theme magit lua-mode kaolin-themes julia-repl julia-mode json-mode js2-mode js-comint imenu-anywhere iedit ibuffer-vc htmlize guide-key go-mode fullframe fold-dwim-org flymake-rust flycheck-rust flycheck-clojure flx expand-region exec-path-from-shell esup emmet-mode elpy dired-single darkroom cyberpunk-2019-theme csv-mode creamsody-theme counsel coffee-mode cargo auctex anzu all-the-icons aggressive-indent adaptive-wrap ace-jump-mode ace-isearch))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
