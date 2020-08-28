;;; ../code/dotfiles/.doom.d/autoload/misc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun insert-formatted-time (format)
  "Insert a timestamp matching a specific format"
  (insert (format-time-string format (current-time))))

;;;###autoload
(defun insert-timestamp-long ()
  "Insert a LONG timestamp"
  (interactive)
  (insert-formatted-time "%a %b %d %H:%M:%S %Z %Y"))

;;;###autoload
(defun insert-timestamp-date ()
  "Insert a plain date"
  (interactive)
  (insert-formatted-time "%Y-%m-%d"))

;;;###autoload
(defun insert-timestamp-time ()
  "Insert a plain timestamp"
  (interactive)
  (insert-formatted-time "%H:%M:%S"))

;;;###autoload
(defun repoutil (command)
  (let ((cmd (format "repoutil %s" command))
        (temp-buf-name "*repoutil*"))
    (get-buffer-create temp-buf-name)
    (shell-command cmd temp-buf-name)
    (switch-to-buffer-other-window temp-buf-name)
    (special-mode)
    (evil-insert 1)))

;;;###autoload
(defun cd/repo/branchstat () (interactive) (repoutil "branchstat"))

;;;###autoload
(defun cd/repo/list () (interactive) (repoutil "list"))

;;;###autoload
(defun cd/repo/fetch () (interactive) (repoutil "fetch"))

;;;###autoload
(defun cd/repo/unclean () (interactive) (repoutil "unclean"))

;;;###autoload
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

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
