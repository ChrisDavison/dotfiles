;; This sets up the load path so that we can override it
(package-initialize nil)

;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)

;; Finally, load my Org-mode (literate programming) config file
(org-babel-load-file "~/.emacs.d/Chris.org")

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "3dafeadb813a33031848dfebfa0928e37e7a3c18efefa10f3e9f48d1993598d3" default)))
 '(package-selected-packages
   (quote
    (org-present org-bullets ox-reveal auctex coffee-mode wgrep-ag ag zenburn-theme writeroom-mode virtualenvwrapper switch-window solarized-theme smex smartscan scss-mode sass-mode rust-mode rainbow-mode rainbow-delimiters racket-mode paredit monokai-theme molokai-theme matlab-mode markdown-mode lively key-chord idomenu ido-ubiquitous ibuffer-vc hi2 helm-descbinds guide-key go-mode go-autocomplete fullframe flymake-rust flycheck-rust flycheck-haskell evil-surround evil emmet-mode ein dired+ diminish csv-nav csv-mode auto-compile anzu ace-isearch ac-haskell-process))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
