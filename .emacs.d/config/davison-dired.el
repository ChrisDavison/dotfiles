(require-package 'dired+)
(require-package 'dired-sort)

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%"))))

;; Make dired a bit nicer
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;; Make dired refresh
(global-auto-revert-mode 1)
(setq global-ato-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


(provide 'davison-dired)
