;;; ../code/dotfiles/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun set-wsl-interop ()
  (setenv "WSL_INTEROP" (string-trim (get-string-from-file "~/.wsl_interop"))))

;;;###autoload
(defun wsl-copy (start end)
  (interactive "r")
  (set-wsl-interop)
  (shell-command-on-region start end "win32yank.exe -i")
  (deactivate-mark))

;;;###autoload
(defun wsl-paste ()
  (interactive)
  (let ((clipboard
         (shell-command-to-string "win32yank.exe -o")))
    (set-wsl-interop)
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard))
    (setq clipboard (substring clipboard 0 -1))
    (insert clipboard)))
