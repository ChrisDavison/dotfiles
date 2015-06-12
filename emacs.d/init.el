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
 '(package-selected-packages
   (quote
    (helm-descbinds idomenu smex ido-ubiquitous virtualenvwrapper flycheck-rust flymake-rust rust-mode go-autocomplete go-mode matlab-mode racket-mode lively auto-compile ac-haskell-process hi2 flycheck-haskell scss-mode sass-mode haml-mode emmet-mode csv-nav csv-mode markdown-mode flycheck writeroom-mode dired+ switch-window ibuffer-vc guide-key smartscan ace-isearch anzu key-chord evil-surround evil rainbow-delimiters rainbow-mode paredit solarized-theme diminish fullframe))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
