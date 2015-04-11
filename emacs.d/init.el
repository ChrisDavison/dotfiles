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
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "573e46dadf8c2623256a164831cfe9e42d5c700baed1f8ecd1de0675072e23c2" "d46b5a32439b319eb390f29ae1810d327a2b4ccb348f2018b94ff22f410cb5c4" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" default)))
 '(org-export-backends (quote (html latex md)))
 '(org-tags-column -65))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
