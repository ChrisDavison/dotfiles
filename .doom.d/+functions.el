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
    (message (format "Running: repoutil %s" command))
    (shell-command cmd "*repoutil*")
    (switch-to-buffer-other-window "*repoutil*")
    (special-mode)
    (evil-insert 1)))
(set-popup-rule! "^\\*repoutil\\*" :side 'bottom :size 0.30 :select t :ttl 1)

(defun repoutil-branchstat () (interactive) (repoutil "branchstat"))

(defun repoutil-list () (interactive) (repoutil "list"))

(defun repoutil-fetch () (interactive) (repoutil "fetch") (quit-window))

(defun repoutil-unclean () (interactive) (repoutil "unclean"))

(defun new-in-git ()
  (interactive)
  (get-buffer-create "*new-in-repo*")
  (shell-command "new_in_git 1" "*new-in-repo*")
  (switch-to-buffer-other-window "*new-in-repo*")
  (special-mode))
(set-popup-rule! "^\\*new-in-repo\\*" :side 'bottom :size 0.30 :select t :ttl 1)

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

;;; Count headers in an org-mode file
(defun headercount (&optional level)
  (interactive)
  (save-excursion
    (let* ((stars (if level (s-repeat level "\*") "\*+"))
           (reg (concat "^" stars " "))
           (n-headers (count-matches reg (point-min) (point-max)))
           (level-str (if level (format " level ≤%d" level) ""))
           (msg (format "%d%s headers" n-headers level-str "headers")))
      (message msg))))
