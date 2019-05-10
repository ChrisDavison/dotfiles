;;; cd-appearance.el --- Appearance of my emacs config

;;; Commentary:

;;; Code:



(when (member "CamingoCode" (font-family-list))
  (set-default-font "CamingoCode"))
(setq line-spacing 0.1)
(setq cd-font-height
      (cond
       (*is-windows* 160)
       (t 200)))
(set-face-attribute 'default nil :height cd-font-height)

;; colour theme
;; Disable themes before loading a new theme
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))
(setq custom-safe-themes t)

;; I always end up going back to solarized, but have been using 'github' within
;; VIM, so should look into alternatives.  Also added functions to quickly jump
;; between light and dark themes.

(use-package solarized-theme :ensure t)
(use-package molokai-theme :ensure t)
(use-package seoul256-theme :ensure t)
(use-package gruvbox-theme :ensure t)

;; *maybe* suspend frame
;; This is useful if running a mac, so that C-z wont hide it.  Considered
;; 'appearance' as I want to see windows.
(defun maybe-suspend-frame ()
  "Don't suspend the frame if it's a Mac."
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))
(global-set-key (kbd "C-z") 'maybe-suspend-frame)

;; suppress gui features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Hide tool bar,  scroll bar and borders
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; highlight current line and no blinking cursor
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(setq linum-format "%d ")

;; alias for yes-or-no
;; This is again a quality of life thing, to allow you to hit yes or no by using
;; y or n.
(defalias 'yes-or-no-p 'y-or-n-p)

;; faster keystroke echoing (setq echo-keystrokes 0.1)

;; tabs to spaces, and tab-related things
(setq tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; show line and column number in status bar
(line-number-mode 1)
(column-number-mode 1)

;; stop scrolling jumping multiple lines
(setq scroll-step           1
      scroll-conservatively 10000)

;; scratch buffer - initial message
(setq-default initial-scratch-message ";; Scratch pad\n\n")

;; font-locking
;; Replace any occurrence of 'lambda' with the actual symbol.
(global-prettify-symbols-mode +1)

(provide 'cd-appearance)
;;; cd-appearance.el ends here
