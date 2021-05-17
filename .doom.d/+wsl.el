;;; ../code/dotfiles/.doom.d/+wsl.el -*- lexical-binding: t; -*-

;;; WSL - Windows Subsystem for Linux
;; workaround to get the right WSL interop variable for clipboard usage
;; used in combination with a shell alias to export $WSL_INTEROP to a file
;; before calling emacs

(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "win32yank.exe -i")
  (deactivate-mark))

(defun wsl-paste ()
  (interactive)
  (let ((clipboard
         (shell-command-to-string "win32yank.exe -o")))
    (insert (substring (replace-regexp-in-string "\r" "" clipboard) 0 -1))))

(defun wsl_interop ()
  (interactive)
  (when (string-match ".*microsoft.*" (shell-command-to-string "uname -a"))
    (setenv "WSL_INTEROP" (string-trim (shell-command-to-string "cat ~/.wsl_interop")))
    (setq is-wsl? t
          browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
          browse-url-generic-args '("/c" "start")
          browse-url-browser-function #'browse-url-generic
          x-selection-timeout 10)
    ;; (message (concat "WSL Interop setup: " (getenv "WSL_INTEROP")))
    ))

(shell-command "wsl_interop_setup")
(wsl_interop)
