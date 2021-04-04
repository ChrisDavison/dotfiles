;;; ../code/dotfiles/.doom.d/autoload/misc.el -*- lexical-binding: t; -*-


;;; Insert timestamps

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



;;; TAGSEARCH
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
(set-popup-rule! "^\\*tagsearch" :side 'bottom :size 0.30 :select t :ttl 1)

;;; REPOUTIL
(defun repoutil (command)
  (let ((cmd (format "repoutil %s" command)))
    (get-buffer-create "*repoutil*")
    (shell-command cmd "*repoutil*")
    (switch-to-buffer-other-window "*repoutil*")
    (special-mode)
    (evil-insert 1)))
(set-popup-rule! "^\\*repoutil\\*" :side 'bottom :size 0.30 :select t :ttl 1)

(defun repoutil-branchstat () (interactive) (repoutil "branchstat"))

(defun repoutil-list () (interactive) (repoutil "list"))

(defun repoutil-fetch () (interactive) (repoutil "fetch"))

(defun repoutil-unclean () (interactive) (repoutil "unclean"))

;;; File utilities

(defun read-file-to-string (filePath)
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
           (direction (if backward -1 1))
           (pos (mod (+ (cl-position file files :test 'equal) direction)
                     (length files))))
      (find-file (nth pos files)))))

(defun find-previous-file ()
  "Find the next file (by name) in the current directory."
  (interactive)
  (find-next-file t))

(defun files-in-curdir-with-ext (ext)
  (let* ((curdir (expand-file-name default-directory))
         (files (directory-files curdir)))
    (seq-filter
     (lambda (filename)
       (s-equals? ext (file-name-extension filename)))
     (-map (lambda (file) (s-concat curdir file)) files))))


(defun erase-all-matches-from-start (regex)
  (replace-regexp regex "" nil (point-min) (point-max)))

(defun zen-biblio ()
  "A function to read and insert a zen-guitar style bibliography."
  (interactive)
  (let ((author (read-string "Authors: "))
        (title (read-string "Title: "))
        (city (read-string "City: "))
        (publisher (read-string "Publisher: "))
        (year (read-string "Year: ")))
    (insert (format "%s. /%s/. %s: %s, %s.\n\n" author title city publisher year))))

;;; Tags (like tagsearch or roam)
(defun tagify (str)
  (interactive "M")
  (s-join " " (--map (format "@%s" it) (s-split " " str))))

(defun roam-tagify (str)
  (interactive "Mtags: ")
  (evil-open-below 1)
  (insert (format "#+ROAM_TAGS: %s\n\n" str))
  (insert (tagify str))
  (evil-force-normal-state)
  (save-buffer))

(defun roam-tagify-toplevel (str)
  (interactive "Mtags: ")
  (evil-goto-first-line)
  (evil-insert-line 1)
  (insert (s-concat "#+ROAM_TAGS: " (tagify str) "\n\n"))
  (evil-force-normal-state)
  (save-buffer))


;;; Lists and checkboxes
(defun make-into-list ()
  "Basically equivalent to org-ctrl-c-minus."
  (interactive)
  (replace-regexp "^" "- " nil (region-beginning) (region-end)))

(defun make-into-checkbox-list ()
  "Convert selection to list (only at root level) of checkboxes."
  (interactive)
  (let ((re (rx bol (zero-or-one "-") (one-or-more space))))
    (replace-regexp re "- [ ] " nil (region-beginning) (region-end))))


;;; Navigate narrows

(defun change-narrow (direction)
  (interactive)
  (progn
    (beginning-of-buffer)
    (widen)
    (if (eq direction 'prev)
        (outline-previous-heading)
      (outline-next-heading))
    (org-narrow-to-subtree)))

(defun move-to-previous-narrow ()
  (interactive)
  (change-narrow 'prev))

(defun move-to-next-narrow ()
  (interactive)
  (change-narrow 'next))


;;; Org-mode functions

(require 'org-id)
(defun org-id-every-heading ()
  (interactive)
  (goto-char (point-max))
  (while (outline-previous-heading)
    (org-id-get-create)))

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

(defvar cd/regex-for-org-property
  (rx bol ;; beginning of line
      (0+ space) ":" (0+ alnum) ":" ;; the ':SOMETHING:'
      (0+ anything) ;; the 'anything'
      "\n")
  "Regexp representing ':SOMETHING: anything'")

(defun remove-org-mode-properties ()
  (interactive)
  (goto-char (point-min))
  (let ((re (rx (0+ space) ))))
  (query-replace-regexp cd/regex-for-org-property ""))

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

(defun set-file-as-main-work-project ()
  (interactive)
  (setq cd/current-work-project (file-name-nondirectory (buffer-file-name))))


;;; UNORGANISED
(defun zsh ()
  (interactive)
  (term "/usr/bin/zsh"))

(defun elpy-send-contiguous-block ()
  (interactive)
  (mark-paragraph)
  (elpy-shell-send-region-or-buffer)
  (evil-forward-paragraph))

;;; Emacs lisp
(defun eval-into-comment ()
  (interactive)
  (let ((sexp (elisp--preceding-sexp)))
    (save-excursion
      (goto-char (line-end-position))
      (delete-horizontal-space)
      (insert " ;; " (prin1-to-string (eval sexp))))))
