;; package -- Summary

;;; Commentary:
;; Modifications to Emacs settings

;;; Code:

;; Load my theme of choice
(load-theme 'sanityinc-solarized-light)


;;------------------------------------------------------------------------------
;; Enable EVIL mode, surround and leader
;;------------------------------------------------------------------------------
(require 'evil-surround)
(require 'evil-leader)

(global-evil-surround-mode)
(global-evil-leader-mode)

(require 'evil)
(evil-mode 1)

;;------------------------------------------------------------------------------
;; Emacs usability settings
;;------------------------------------------------------------------------------

;; Convert tabs to spaces
(setq-default indent-tabs-mode nil)

;; Show line and column num in the mode bar
(line-number-mode 1)
(linum-mode)
(column-number-mode 1)

;; Highlight current line and don't blink the cursor
(global-hl-line-mode 1)
(blink-cursor-mode 0)

;; Don't type so much when emacs asks for confirmation
(defalias 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Allow backups, and move them into their own directory
(setq make-backup-files t)
(setq version-control nil)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;;------------------------------------------------------------------------------
;; Emacs key bindings
;;------------------------------------------------------------------------------

;; Use Control-HJKL to move between splits (VI style)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)


;; Code for vi-style 'leader' mode.
;; Only works in <N> (normal) VIM mode, not insert.
(evil-leader/set-leader "\\")
(evil-leader/set-key "e" 'find-file)
(evil-leader/set-key "d" 'dired)
(evil-leader/set-key "b" 'switch-to-buffer)
(evil-leader/set-key "k" 'kill-buffer)

;;------------------------------------------------------------------------------
;; Enable ;; as VIM escape
;;------------------------------------------------------------------------------
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global ";;" 'evil-normal-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'chris-config)

;;; chris-config.el ends here
