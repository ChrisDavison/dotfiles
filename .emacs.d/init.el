;;; init --- My emacs configuration

;;; Commentary:

;;; Code:
(load "~/code/dotfiles/.emacs.d/chris-davison.el" t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(package-selected-packages
   (quote
    (yasnippet-snippets julia-repl julia-mode org-bullets org-bullets-mode uniquify ace-isearch auctex rust-mode haml-mode markdown-mode s ivy fold-dwim smartparens undo-tree virtualenvwrapper use-package tao-theme switch-window solarized-theme smartscan seoul256-theme scss-mode sass-mode rainbow-mode pandoc-mode nodejs-repl magit lua-mode key-chord json-mode js2-mode js-comint ibuffer-vc htmlize guide-key go-mode fullframe fold-dwim-org flymake-rust flycheck-rust flycheck-clojure f expand-region exec-path-from-shell evil-surround evil-smartparens evil-leader emmet-mode elpy diminish csv-mode counsel color-theme-sanityinc-tomorrow coffee-mode cargo anzu aggressive-indent ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
