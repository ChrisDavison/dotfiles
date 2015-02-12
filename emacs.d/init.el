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
    ("cee3ced547529a0923830318df23cb329255b963e39951d79fd7e56c54b0ade3" "e74d80bf86c7951b1a27994faa417f7e3b4a02f7a365ed224f032bd29f5d2d6d" "fa942713c74b5ad27893e72ed8dccf791c9d39e5e7336e52d76e7125bfa51d4c" "87755ed88c91cd87ee37b666b82edeb382b3a3a6191078a56bec558ebf8a58d9" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "b458d10c9ea0c8c465635b7b13e1bd23f04e6b696b1ca96cb2c4eca35a31641e" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "acb039d6f2c41b3bd852b448351b2979f44ef488026c95dd5228d2f6da57f574" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "72407995e2f9932fda3347e44e8c3f29879c5ed88da71f06ba4887b0596959a4" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "573e46dadf8c2623256a164831cfe9e42d5c700baed1f8ecd1de0675072e23c2" "4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "536c5fc0de7ac2a7ba53b072286fcf09cbd05eb53e84eca9fc9f743837bfb42c" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
