(require-package 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jk" 'evil-normal-state)
(key-chord-define-global "gc" 'comment-or-uncomment-region)

(provide 'davison-keychord)
