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
    (org-bullets doom-themes rainbow-delimiters rotate deadgrep helpful visual-fill-column fold-dwim-org fold-dwim yafolding yasnippet-snippets use-package switch-window sass-mode racer pyvenv org-download org-beautify-theme js2-mode imenu-anywhere iedit hydra htmlize guide-key go-mode fullframe forge flymake-rust flycheck-rust expand-region exec-path-from-shell dumb-jump doom-modeline doneburn-theme diminish darkokai-theme counsel company-quickhelp company-anaconda cargo avy anzu aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
