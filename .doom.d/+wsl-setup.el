;;; ../code/dotfiles/.doom.d/+wsl-setup.el -*- lexical-binding: t; -*-
;; workaround to get the right WSL interop variable for clipboard usage
;; used in combination with a shell alias to export $WSL_INTEROP to a file
;; before calling emacs
(defun wsl-browse-url (url &optional _new-window)
  ;; new-window ignored
  "Opens link via powershell.exe"
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((quotedUrl (format "start '%s'" url))
        (powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"))
    (apply 'call-process powershell nil t nil (list "-Command" quotedUrl))))

(when is-wsl?
  (setenv "WSL_INTEROP" (string-trim (shell-command-to-string "cat ~/.wsl_interop")))
  (setq browse-url-browser-function 'wsl-browse-url))
