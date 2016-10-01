;;; my-progress-bar --- Progress bar for debugging slow emacs.d

;;; Commentary:

;;; Code:

;; Progress bar & logs
(defvar ///-steps 7 "Number of calls to `///'.")
(defvar ///-counter 0 "Initializing the counter.")
(defvar ///-time (current-time) "Time counter.")
(defvar ///-previous nil "Previous loaded package.")
(defun /// (&optional title)
  "A progress bar on the mode-line and TITLE in the echo area."
  (redisplay)
  (when ///-previous
    (message "        done in %.3fs\n"
             (float-time (time-subtract (current-time) ///-time))))
  (when title (message "Loading %s" title))
  (setq
   ///-time (current-time)
   ///-previous title
   mode-line-format (make-string
                     (* ///-counter (/ (window-total-size nil 'width)
                                       ///-steps))
                     ?|)
   ///-counter (1+ ///-counter)))
(add-hook 'after-init-hook '///)

(provide 'my-progress-bar)
;;; my-progress-bar ends here
