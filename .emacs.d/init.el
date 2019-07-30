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
    (company-try-hard hide-lines rainbow-delimiters rotate helpful visual-fill-column fold-dwim-org fold-dwim yafolding doom-modeline doom-themes darkokai-theme doneburn-theme hydra org-download org-bullets htmlize exec-path-from-shell switch-window avy dumb-jump imenu-anywhere counsel ivy js2-mode sass-mode company-anaconda anaconda-mode pyvenv racer cargo flycheck-rust flymake-rust rust-mode go-mode flycheck company-quickhelp company anzu aggressive-indent yasnippet-snippets yasnippet iedit expand-region forge magit guide-key diminish f s fullframe use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
