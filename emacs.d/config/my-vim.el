;;; my-vim.el --- Prefer the modal editing of vim, with the ecosystem of emacs

;;; Commentary:
;;;
;;; Being a VIM user...Emacs' keybindings are quite nasty.  As such, I
;;; try to make this editing experience as close to the VIM experience
;;; as possible, while allowing for the nicety of Emacs.  Key-chord is
;;; pretty nice to keep my key presses down.

;;; Code:
(use-package evil :ensure t
  :config (evil-mode 1))

(use-package evil-surround :ensure t
  :config (global-evil-surround-mode))

(use-package evil-leader :ensure t 
  :config (global-evil-leader-mode))

(use-package key-chord :ensure t
  :config (key-chord-mode 1))

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)

(key-chord-define-global "jk" 'evil-normal-state)
(key-chord-define-global "gc" 'comment-or-uncomment-region)

;; Some of these functions are only pulled in later
;; But VIM is added early incase customisation breaks.
(evil-leader/set-key
  "w" 'save-buffer
  "f" 'counsel-find-file
  "i" 'counsel-imenu
  "h" 'ivy-switch-buffer
  "s" 'swiper
  "j" 'jump-to-register
  "k" 'kill-buffer)

(provide 'my-vim)
;;; my-vim.el ends here
