;;; ../code/dotfiles/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun set-wsl-interop ()
  (setenv "WSL_INTEROP" (string-trim (get-string-from-file "~/.wsl_interop"))))

;;;###autoload
(defun wsl-copy (start end)
  (interactive "r")
  (setenv "WSL_INTEROP" (string-trim (get-string-from-file "~/.wsl_interop")))
  (shell-command-on-region start end "win32yank.exe -i")
  (deactivate-mark))

;;;###autoload
(defun wsl-paste ()
  (interactive)
  (setenv "WSL_INTEROP" (string-trim (get-string-from-file "~/.wsl_interop")))
  (let ((clipboard
         (shell-command-to-string "win32yank.exe -o")))
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard))
    (setq clipboard (substring clipboard 0 -1))
    (insert clipboard)))

;;;###autoload
(defun emoji-heading (fontfunc fonticon headingname)
  (format "%s %s"
          (funcall fontfunc fonticon :face 'all-the-icons-green :v-adjust 0.01)
          headingname))

;;;###autoload
(defun unpackaged/org-fix-blank-lines (prefix)
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
(defun fish-term ()
  (interactive)
  (term "/usr/bin/fish"))

;;;###autoload
(defun cd/org-file-from-subtree (filename)
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
    ;; (org-back-to-heading)

    ;; Copy current subtree into clipboard
    (org-cut-subtree)

    ;; ;; Delete everything but the headline
    ;; (org-mark-subtree)
    ;; (org-next-visible-heading 1)
    ;; (call-interactively 'delete-region)
    ;; (org-previous-visible-heading 1)

    ;; ;; Mark the current headline text
    ;; (org-end-of-line)
    ;; (call-interactively 'set-mark-command)
    ;; (org-beginning-of-line)

    ;; Convert headline to a link of the to-be-created file
    (org-insert-link nil filename title)

    (with-temp-file filename
      (org-mode)
      (insert "#+TITLE: " title "\n\n")
      (org-paste-subtree))))

;;;###autoload
(defun cd/org-file-from-selection ()
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
(defun cd/org-open-link-same ()
  (interactive)
  (let ((old-setup org-link-frame-setup))
    (setq org-link-frame-setup '((file . find-file)))
    (org-open-at-point)
    (setq org-link-frame-setup old-setup)))

;;;###autoload
(defun cd/refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))

;;;###autoload
(defun cd/refile-to-file (&optional target)
  (interactive)
  (let ((filename (or target (read-file-name "Refile to: ")))
        (old-refile-targets org-refile-targets))
    (progn (setq org-refile-targets `((filename . (:maxlevel . 6))))
           (org-refile)
           (setq org-refile-targets old-refile-targets))))

;;;###autoload
(defun cd/refile-to-this-file ()
  (interactive)
  (cd/refile-to-file (buffer-name)))

;;;###autoload
(defun cd/rg-journal (search)
  (interactive "Msearch string: ")
  (rg search "org" org-journal-dir))

;;;###autoload
(defun cd/rg-org (search)
  (interactive "Msearch string: ")
  (rg search "org" org-directory))

;;;###autoload
(defun change-state-and-archive ()
  (interactive)
  (org-todo)
  (org-archive-subtree-default))

;;;###autoload
(defun cd/set-theme-dark ()
  (interactive)
  (setq doom-theme cd/dark-theme)
  (doom/reload-theme))

;;;###autoload
(defun cd/set-theme-light ()
  (interactive)
  (setq doom-theme cd/light-theme)
  (doom/reload-theme))

;;;###autoload
(defun cd/reload-config ()
  (interactive)
  (load (expand-file-name "~/.doom.d/config.el")))
