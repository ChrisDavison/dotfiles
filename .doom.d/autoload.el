;;; ../code/dotfiles/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun next-circular-index (i n &optional reverse)
  (let ((func (if reverse '- '+)))
    (mod (funcall func i 1) n)))

;;;###autoload
(defun fish-term ()
  (interactive)
  (term "/usr/bin/fish"))

;;;###autoload
(defun jump-to-here-anchor ()
  (interactive)
  (goto-char (point-min))
  (search-forward "<<here>>"))

;;;###autoload
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;###autoload
(defun run-julia ()
  "Launch julia in a term buffer."
  (interactive)
  (set-buffer (make-term "julia" "julia"))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*julia*"))

;;;###autoload
(defun jump-to-work-project ()
  (interactive)
  (org-roam-find-file "@work "))

