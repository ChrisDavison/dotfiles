;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)


;; (defadvice desktop-read (around trace-desktop-errors activate)
;;   (let ((debug-on-error t))
;;     ad-do-it))

;; (defadvice desktop-read (around time-restore activate)
;;     (let ((start-time (current-time)))
;;       (prog1
;;           ad-do-it
;;         (message "Desktop restored in %.2fms"
;;                  (sanityinc/time-subtract-millis (current-time)
;;                                                  start-time)))))

;; (defadvice desktop-create-buffer (around time-create activate)
;;   (let ((start-time (current-time))
;;         (filename (ad-get-arg 1)))
;;     (prog1
;;         ad-do-it
;;       (message "Desktop: %.2fms to restore %s"
;;                (sanityinc/time-subtract-millis (current-time)
;;                                                start-time)
;;                (when filename
;;		 (abbreviate-file-name filename))))))

;; Allow backups, and move them into their own directory
(setq make-backup-files t)
(setq version-control nil)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))


(provide 'davison-sessions)
