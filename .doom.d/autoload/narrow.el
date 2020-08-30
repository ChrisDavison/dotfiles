;;; ../code/dotfiles/.doom.d/autoload/narrow.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cd/move-to-previous-narrow ()
  (interactive)
  (progn
    (beginning-of-buffer)
    (widen)
    (outline-previous-heading)
    (org-narrow-to-subtree)))

;;;###autoload
(defun cd/move-to-next-narrow ()
  (interactive)
  (progn
    (beginning-of-buffer)
    (widen)
    (outline-next-heading)
    (org-narrow-to-subtree)))
