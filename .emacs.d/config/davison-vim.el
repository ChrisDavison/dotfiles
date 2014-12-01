(require-package 'evil)
(evil-mode 1)

(require-package 'evil-surround)
(global-evil-surround-mode)

; Use Control-HJKL to move between splits (VI style)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(provide 'davison-vim)
