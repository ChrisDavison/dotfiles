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
