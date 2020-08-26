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

(defun cd/repo/branchstat () (interactive) (repoutil "branchstat"))
(defun cd/repo/list () (interactive) (repoutil "list"))
(defun cd/repo/fetch () (interactive) (repoutil "fetch"))
(defun cd/repo/unclean () (interactive) (repoutil "unclean"))
