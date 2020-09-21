;;; ../code/dotfiles/.doom.d/autoload/misc.el -*- lexical-binding: t; -*-

(defun insert-formatted-time (format)
  "Insert a timestamp matching a specific format"
  (insert (format-time-string format (current-time))))

(defun insert-timestamp-long ()
  "Insert a LONG timestamp"
  (interactive)
  (insert-formatted-time "%a %b %d %H:%M:%S %Z %Y"))

(defun insert-timestamp-date ()
  "Insert a plain date"
  (interactive)
  (insert-formatted-time "%Y-%m-%d"))

(defun insert-timestamp-time ()
  "Insert a plain timestamp"
  (interactive)
  (insert-formatted-time "%H:%M:%S"))

(defun tagsearch-list (&optional tags)
  "List tags under the current directory.

When optional TAGS is a string, show only files matching those tags"
  (interactive)
  (let ((cmd (concat "tagsearch " (or tags "")))
        (temp-buf-name "*tagsearch*"))
    (get-buffer-create temp-buf-name)
    (shell-command cmd temp-buf-name)
    (switch-to-buffer-other-window temp-buf-name)
    (special-mode)
    (evil-insert 1)))

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

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun find-next-file (&optional backward)
  "Find the next file (by name) in the current directory.

With prefix arg, find the previous file."
  (interactive "P")
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                (sort (directory-files (file-name-directory file) t nil t) 'string<)))
           (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                     (length files))))
      (find-file (nth pos files)))))

(defun find-next-md-org-pair (&optional backward)
  "Find the next file (by name) in the current directory.

With prefix arg, find the previous file."
  (interactive "P")
  (when buffer-file-name
    (let* ((file (if (s-equals? "md" (file-name-extension buffer-file-name))
                     buffer-file-name
                   (substring buffer-file-name 0 -4)))
           (files (files-in-curdir-with-ext "md"))
           (direction (if backward -1 1))
           (pos (mod (+ (cl-position file files :test 'equal) direction)
                     (length files))))
      (delete-other-windows)
      (find-file (nth pos files))
      (find-file-other-window (s-concat (buffer-file-name (find-file (nth pos files))) ".org")))))

(defun files-in-curdir-with-ext (ext)
  (let* ((curdir (expand-file-name default-directory))
         (files (directory-files curdir)))
    (seq-filter
     (lambda (filename)
       (s-equals? ext (file-name-extension filename)))
     (-map (lambda (file) (s-concat curdir file)) files))))

(defun remove-org-mode-properties ()
  (interactive)
  (goto-char (point-min))
  (query-replace-regexp " *:.*:.*\n" ""))

(defun erase-all-matches-from-start (regex)
  (replace-regexp regex "" nil (point-min) (point-max)))

(defun cd/fix-org-from-evernote ()
  (interactive)
  (erase-all-matches-from-start " *:.*:.*\n")
  (erase-all-matches-from-start "\\[\\]{#[0-9]+}\n\n")
  (erase-all-matches-from-start "\\[")
  (erase-all-matches-from-start "\\]")
  (erase-all-matches-from-start "\\\\")
  (replace-regexp " " " " nil (point-min) (point-max))
  (erase-all-matches-from-start " ")
  (goto-char (point-min)))
