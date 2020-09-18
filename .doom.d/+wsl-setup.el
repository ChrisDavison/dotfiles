;;; ../code/dotfiles/.doom.d/+wsl-setup.el -*- lexical-binding: t; -*-
;; workaround to get the right WSL interop variable for clipboard usage
;; used in combination with a shell alias to export $WSL_INTEROP to a file
;; before calling emacs
(after! s
  (when (s-contains? (shell-command-to-string "uname -a") "microsoft")
    (set-wsl-interop)
        (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "~/bin/firefox")
    (setq x-selection-timeout 10)))
