;;; ../code/dotfiles/.doom.d/+literature_capture.el -*- lexical-binding: t; -*-

;;; org-capture for literature
(defun read-capitalized-title ()
  (s-titleize (read-string "Title: ")))

(defun read-author ()
  (let ((name (read-string "Author: " "" nil nil)))
    (if (s-equals? name "")
        nil
      (format-author-name name))))

(defun format-author-name (author)
  (concat (seq-mapcat
           (lambda (author-part)
             (if (> (length author-part) 1)
                 (s-concat " " (s-capitalize author-part))
               (s-concat (s-capitalize author-part) ".")))
           (s-split " " author))))

(defun maybe-get-bibtex ()
  "Maybe get a DOI number for a reference"
  (let ((doi (read-string "DOI: " "" nil nil)))
    (if (s-equals? doi "")
        nil
      (s-concat ("\n")))))

(defun read-authors ()
  (setq authors (read-author)
        running t)
  (while running
    (setq input (read-author))
    (if (s-equals? input nil)
        (setq running nil)
      (setq authors (concat authors " and " input))))
  authors)

