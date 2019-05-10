;;; cd-init --- Initialisation stuff for my emacs config

;;; Commentary:

;;; Code:

;; consts

;; Basically give a useful const to check if running OSX.  This may be useful
;; for your config, to override platform-specific behaviour (or perhaps
;; implement some more specificity).
(defconst *spell-check-support-enabled* nil)
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-windows* (equal system-type 'windows-nt))

;; eval after load
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(setq package-enable-at-startup nil)
(package-initialize)

(use-package fullframe :ensure t
  :config (fullframe list-packages quit-window))

(use-package cl-lib :ensure t
  :config (require 'cl-lib))

(use-package diminish :ensure t)

(use-package let-alist :ensure t)

(setq apropos-do-all t)

(provide 'cd-init)
;;; cd-init ends here
