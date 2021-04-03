;;; ../code/dotfiles/.doom.d/autoload.el -*- lexical-binding: t; -*-

;; -------
;; UTILITY
;; -------

;;;###autoload
(defun insert-newline-if-not-at-start ()
  (unless (= (point) (line-beginning-position))
    (newline)))

;; --------
;; ORG MODE
;; --------

;;;###autoload
(defun org-file-from-subtree (filename)
  "Take the current subtree and create a new file from
  it. Replace the current subtree with its main heading (i.e.,
  delete all of its childen), and make the heading into a link
  to the newly created file,

In the new file, promote all direct children of the original
  subtree to be level 1-headings, and transform the original
  heading into the '#+TITLE' parameter.

If called with the universal argument, prompt for new filename,
otherwise use the subtree title."
  (interactive "F")
  (let ((filename (concat "~/" (file-relative-name filename "~")))
        (title (s-capitalized-words (s-replace "-" " " (file-name-sans-extension (file-name-base filename))))))
    ;; Copy current subtree into clipboard
    (org-cut-subtree)

    ;; Convert headline to a link of the to-be-created file
    (org-insert-link nil filename title)

    (with-temp-file filename
      (org-mode)
      (insert "#+TITLE: " title "\n\n")
      (org-paste-subtree))))

;;;###autoload
(defun org-file-from-selection ()
  "Create a new file from current selection, inserting a link.

  Prompt for a filename, and create. Prompt for an org-mode
  TITLE, and insert. Insert the cut region. Then, insert the link
  into the source document, using TITLE as description"
  (interactive)
  (when (region-active-p)
    (let* ((filename (read-file-name "New filename: " org-directory))
           (file-relative (file-relative-name
                           filename
                           (file-name-directory (expand-file-name filename))))
           (title (read-from-minibuffer "Title: ")))
      (call-interactively' kill-region)
      (insert (format "[[file:%s][%s]]" file-relative title))
      (with-temp-file filename
        (org-mode)
        (insert (concat "#+TITLE: " title "\n\n"))
        (evil-paste-after 1)))))

;;;###autoload
(defun org-open-link-same-window ()
  (interactive)
  (let ((old-setup org-link-frame-setup))
    (setq org-link-frame-setup '((file . find-file)))
    (org-open-at-point)
    (setq org-link-frame-setup old-setup)))

;;;###autoload
(defun org-refile-to-file (&optional target)
  (interactive)
  (let ((filename (or target (ivy-read "Refile to: " (f-files default-directory nil t))))
        (old-refile-targets org-refile-targets))
    (progn (setq org-refile-targets `((,filename . (:maxlevel . 6))))
           (org-refile)
           (setq org-refile-targets old-refile-targets))))

;;;###autoload
(defun org-refile-to-this-file ()
  (interactive)
  (org-refile-to-file (buffer-name)))

;;;###autoload
(defun org-change-state-and-archive ()
  (interactive)
  (org-todo)
  (org-archive-subtree-default))

;;;###autoload
(defun org-paste-checkbox-list ()
  (interactive)
  (insert-newline-if-not-at-start)
  (insert (replace-regexp-in-string "^" "- [ ] " (current-kill 0))))

;;;###autoload
(defun org-paste-todo-header-list (&optional level)
  (interactive)
  (let* ((level (or level 1))
         (stars (s-repeat level "*"))
         (todo (s-concat stars " TODO ")))
    (insert-newline-if-not-at-start)
    (insert (replace-regexp-in-string "^" todo (current-kill 0)))))

;;;###autoload
(defun org-paste-todo-header-list-l2 ()
  (interactive)
  (org-paste-todo-header-list 2))

;;;###autoload
(defun org-paste-todo-header-list-l3 ()
  (interactive)
  (org-paste-todo-header-list 3))

;;;###autoload
(defun org-archive-level1-done ()
  (interactive)
  (save-excursion
    (goto-char 1)
    (org-archive-all-done)))

;;;###autoload
(defun org-archive-done-under-subtree ()
  (interactive)
  (org-archive-all-done))

;;;###autoload
(defun org-copy-link-url (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
          (type (org-element-property :type link))
          (url (org-element-property :path link))
          (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

;;;###autoload
(defun org-fix-blank-lines (prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))

;;;###autoload
(defun my-refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))



;; -------------
;; Ripgrep stuff
;; -------------

;;;###autoload
(defun rg-journal (search)
  (interactive "Msearch string: ")
  (rg search "org" "~/code/knowledge/journal"))

;;;###autoload
(defun rg-logbook (search)
  (interactive "Msearch string: ")
  (rg search "org" "~/code/knowledge/logbook/"))

;;;###autoload
(defun rg-org (search)
  (interactive "Msearch string: ")
  (rg search "org" org-directory))

;; ------------------------------------
;; Jump to journals and logbook entries
;; ------------------------------------
;;;###autoload
(defun logbook ()
  "Jump to the logbook entry for today, or create if it doesn't exist."
  (interactive)
  (let ((org-journal-dir "~/code/knowledge/logbook"))
    (org-journal-new-entry nil)))

;;;###autoload
(defun journal ()
  "Jump to the journal entry for today, or create if it doesn't exist."
  (interactive)
  (let ((org-journal-dir "~/code/knowledge/journal"))
    (org-journal-new-entry nil)))

;;;###autoload
(defun jump-to-journal (journal-prefix)
  (let* ((time-string (concat journal-prefix "-%Y.org"))
        (filename (format-time-string time-string))
        (filepath (f-join org-directory filename))
        (old-journal-format org-journal-file-format))
    (find-file filepath)
    (goto-char (point-min))
    ;; if header doesn't exist, search-forward will return (point-min)
    ;; so create a new journal entry
    (when (not (search-forward (format-time-string "%F %A") nil t))
      (setq org-journal-file-format filename)
      (org-journal-new-entry nil)
      (setq org-journal-file-format old-journal-format))))

;;;###autoload
(defun jump-to-logbook ()
  (interactive)
  (let* ((filename (format-time-string "%Y.org"))
        (filepath (f-join "~/code/knowledge/logbook" filename))
        (old-journal-format org-journal-file-format))
    (find-file filepath)
    (goto-char (point-min))
    ;; if header doesn't exist, search-forward will return (point-min)
    ;; so create a new journal entry
    (when (not (search-forward (format-time-string "%F %A") nil t))
      (setq org-journal-file-format filename)
      (org-journal-new-entry nil)
      (setq org-journal-file-format old-journal-format))
    (evil-scroll-line-to-center)))

;;;###autoload
(defun jump-to-last-journal ()
  "Go to the last file in the journals folder"
  (interactive)
  (let* ((journals (f-files (format-time-string "~/code/knowledge/journal/%Y/")))
         (previous (nth (- (length journals) 1) journals)))
        (find-file previous)))

;;;###autoload
(defun jump-to-last-logbook ()
  "Go to the last file in the journals folder"
  (interactive)
  (let* ((journals (f-files (format-time-string "~/code/knowledge/logbook/%Y/")))
         (previous (nth (- (length journals) 1) journals)))
        (find-file previous)))

;;----------------------------------------------------------------------------
;;; Themes / appearance
;;----------------------------------------------------------------------------
;;;###autoload
(defun set-theme-dark ()
  (interactive)
  (setq doom-theme (nth 0 theme-preferences-dark))
  (doom/reload-theme))

;;;###autoload
(defun set-theme-light ()
  (interactive)
  (setq doom-theme (nth 0 theme-preferences-light))
  (doom/reload-theme))

;;;###autoload
(defun next-theme-dark ()
  (interactive)
  (next-theme nil theme-preferences-dark))

;;;###autoload
(defun next-theme-light ()
  (interactive)
  (next-theme nil theme-preferences-light))

;;----------------------------------------------------------------------------
;;; Utility
;;----------------------------------------------------------------------------
;;;###autoload
(defun reload-config ()
;;;###autoload
  (interactive)
  (load (expand-file-name "~/.doom.d/config.el")))

;;;###autoload
(defun next-circular-index (i n &optional reverse)
  (let ((func (if reverse '- '+)))
    (mod (funcall func i 1) n)))

;;;###autoload
(defun emoji-heading (fontfunc fonticon headingname)
  (let ((icon (funcall fontfunc fonticon :face 'all-the-icons-green :v-adjust 0.01)))
    (format "%s %s" icon headingname)))

;;;###autoload
(defun fish-term ()
  (interactive)
  (term "/usr/bin/fish"))

;;;###autoload
(defun next-theme (&optional backward alternate-theme-list)
  (interactive)
  (let* ((themes (if alternate-theme-list alternate-theme-list (custom-available-themes)))
         (idx-current (cl-position doom-theme themes))
         (idx-next (next-circular-index (if idx-current idx-current 0) (length themes) (if backward t nil)))
         (next (nth idx-next themes)))
    (message "%s" next)
    (setq doom-theme next)
    (doom/reload-theme)))

;;;###autoload
(defun jump-to-here-anchor ()
  (interactive)
  (goto-char (point-min))
  (search-forward "<<here>>"))

;;;###autoload
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;###autoload
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun org-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (org-fill-paragraph nil region)))

;;;###autoload
(defun find-todays-headline-or-create ()
  (interactive)
  (let* ((today-str (format-time-string "%Y-%m-%d %A"))
         (marker (org-find-exact-headline-in-buffer today-str)))
    (if marker (org-goto-marker-or-bmk marker)
        (progn (goto-char (point-max))
        (org-insert-heading)
        (insert " " today-str)))))

;;;###autoload
(defun run-julia ()
  "Launch julia in a term buffer."
  (interactive)
  (set-buffer (make-term "julia" "julia"))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*julia*"))
