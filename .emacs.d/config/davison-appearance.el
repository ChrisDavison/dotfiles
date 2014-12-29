;; Add some nice themes
(require-package 'solarized-theme)
(require-package 'flatland-theme)
(require-package 'molokai-theme)
;;(require-package 'color-theme-sanityinc-tomorrow)

;; Load my theme of choice
(load-theme 'solarized-dark t)

;; Font height
(set-face-attribute 'default nil :height 120)

;; Easily toggle between light and dark themes
(defun light()
  "Activate a light color theme."
  (interactive)
  (load-theme 'solarized-light))

(defun dark()
  "Activate a dark color theme."
  (interactive)
  (load-theme 'solarized-dark))

;; Stop C-z from minimizing windows under OS X
(defun maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'maybe-suspend-frame)

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

;; Window size and features
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; Highlight current line and don't blink the cursor
(global-hl-line-mode 1)
(blink-cursor-mode 0)

;; Don't type so much when emacs asks for confirmation
(defalias 'yes-or-no-p 'y-or-n-p)

;; Faster repeatkeys
(setq echo-keystrokes 0.1)

;; Convert tabs to spaces
(setq tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Show line and column num in the mode bar
(line-number-mode 1)
(column-number-mode 1)

;; Show parenthesis matching
(show-paren-mode t)

(provide 'davison-appearance)
