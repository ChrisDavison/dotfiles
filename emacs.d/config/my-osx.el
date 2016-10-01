;;; my-osx.el --- Hand-holding for OSX

;;; Commentary:

;;; Code:
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX")
  (use-package exec-path-from-shell :ensure t
    :config
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize)))
  
  ;;Make the mouse wheel/trackpad less jerky
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore)))

  ;;And give emacs some of the expected OS X keybinds
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (after-load 'nxml-mode (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  (global-set-key (kbd "M-<up>") 'toggle-frame-fullscreen) ;;Bind Meta-<UP> to fullscreen toggling
  (global-set-key (kbd "<f10>") 'toggle-frame-fullscreen) ;;Bind Meta-<UP> to fullscreen toggling
  )



(provide 'my-osx)
;;; my-osx.el ends here
