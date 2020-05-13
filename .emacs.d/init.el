;;; init --- My emacs configuration

;;; Commentary:

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.

(setq gc-cons-threshold 100000000)

(require 'org)
(progn
  (let ((cd/config-org "~/code/dotfiles/.emacs.d/chris-davison.org")
        (cd/config-el "~/code/dotfiles/.emacs.d/chris-davison.el"))
    (org-babel-tangle-file cd/config-org cd/config-el)
    (load cd/config-el t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#e8e8e8")
 '(company-quickhelp-color-foreground "#444444")
 '(compilation-message-face (quote default))
 '(electric-indent-mode nil)
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors
   (quote
    (("#323342" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#323342" . 100))))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#8f4e8b" "#8f684e" "#c3a043" "#397460" "#54ab8e" "#20a6ab" "#3573b1" "#DC8CC3")))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   (quote
    (smartparens flycheck-clojure parinfer paredit-mode engine-mode cyberpunk-2019-theme cyberpunk-theme company-racer company-bibtex ob-sh org-ref org-recur projectile hydra-posframe evil-org evil-iedit-state evil-commentary evil deadgrep ivy-hydra deft lsp-mode org-sidebar electric-indent nov dired-single gist undo-tree company-try-hard hide-lines rainbow-delimiters rotate helpful visual-fill-column fold-dwim-org fold-dwim yafolding doom-modeline doom-themes darkokai-theme doneburn-theme hydra org-download org-bullets htmlize exec-path-from-shell switch-window avy dumb-jump imenu-anywhere counsel ivy js2-mode sass-mode company-anaconda anaconda-mode pyvenv racer cargo flycheck-rust flymake-rust rust-mode go-mode flycheck company-quickhelp company anzu aggressive-indent yasnippet-snippets yasnippet iedit expand-region forge magit guide-key diminish f s fullframe use-package)))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(weechat-color-list
   (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "#9C00FF" :weight bold :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 257)) (:foreground "#63de5d")) (((class color) (min-colors 89)) (:foreground "#87D700")))))
