;;; init --- My emacs configuration

;;; Commentary:

;;; Code:
(setq gc-cons-threshold 100000000)
(load "~/code/dotfiles/.emacs.d/chris-davison.el" t)
(message "Startup: %s" (emacs-init-time))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242730" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF" "#bbc2cf"])
 '(fci-rule-color "#62686E")
 '(global-company-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(objed-cursor-color "#ff665c")
 '(package-selected-packages
   (quote
    (adaptive-wrap visual-fill-column creamsody-theme cyberpunk-2019-theme iedit imenu-anywhere dired-single racer rainbow-delimiters zenburn-theme doom-themes material-theme yasnippet-snippets julia-repl julia-mode org-bullets org-bullets-mode uniquify ace-isearch auctex rust-mode haml-mode markdown-mode s ivy fold-dwim smartparens undo-tree virtualenvwrapper use-package tao-theme switch-window solarized-theme smartscan seoul256-theme scss-mode sass-mode rainbow-mode pandoc-mode nodejs-repl magit lua-mode key-chord json-mode js2-mode js-comint ibuffer-vc htmlize guide-key go-mode fullframe fold-dwim-org flymake-rust flycheck-rust flycheck-clojure f expand-region exec-path-from-shell evil-surround evil-smartparens evil-leader emmet-mode elpy diminish csv-mode counsel color-theme-sanityinc-tomorrow coffee-mode cargo anzu aggressive-indent ace-jump-mode)))
 '(vc-annotate-background "#242730")
 '(vc-annotate-color-map
   (list
    (cons 20 "#7bc275")
    (cons 40 "#a6c677")
    (cons 60 "#d1ca79")
    (cons 80 "#FCCE7B")
    (cons 100 "#f4b96e")
    (cons 120 "#eda461")
    (cons 140 "#e69055")
    (cons 160 "#db8981")
    (cons 180 "#d082ae")
    (cons 200 "#C57BDB")
    (cons 220 "#d874b0")
    (cons 240 "#eb6d86")
    (cons 260 "#ff665c")
    (cons 280 "#d15e59")
    (cons 300 "#a35758")
    (cons 320 "#754f56")
    (cons 340 "#62686E")
    (cons 360 "#62686E")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
