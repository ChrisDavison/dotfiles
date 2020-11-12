;;; ../code/dotfiles/.doom.d/+wsl-setup.el -*- lexical-binding: t; -*-
;; workaround to get the right WSL interop variable for clipboard usage
;; used in combination with a shell alias to export $WSL_INTEROP to a file
;; before calling emacs

(when is-wsl?
  (setenv "WSL_INTEROP" (string-trim (shell-command-to-string "cat ~/.wsl_interop")))
  (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function #'browse-url-generic
        x-selection-timeout 10)

  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "win32yank.exe -i")
    (deactivate-mark))

  (defun wsl-paste ()
    (interactive)
    (let ((clipboard
           (shell-command-to-string "win32yank.exe -o")))
      (insert (substring (replace-regexp-in-string "\r" "" clipboard) 0 -1)))))
