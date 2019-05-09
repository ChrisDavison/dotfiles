;;; my-navigation.el --- Navigation and searching

;;; Commentary:
;;;


;;; Code:

;; Show current and total matches while searching
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap query-replace] . anzu-query-replace))
  :config (global-anzu-mode t))

;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)


;; smartscan (Vim *)
;; From https://github.com/itsjeyd/emacs-config/blob/emacs24/init.el
;; This basically allows you to do something similar to VIM *...i.e. it'll jump forward or backward to the next occurence of the symbol under the cursor.
;; Bound to =M-n= and =M-b= by default, I think.
(use-package smartscan :ensure t
             :config (global-smartscan-mode t))


;; Prompt with a hud when switching windows, if more than 2 windows
(use-package switch-window
  :ensure t
  :config (setq switch-window-shortcut-style 'alphabet)
  :bind ("C-x o" . switch-window))

;; Interactively modify buffer list
(use-package fullframe :ensure t)
(after-load 'buffer
  (fullframe ibuffer ibuffer-quit))

(use-package ibuffer-vc :ensure t)

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))


;; Explicitly require ibuffer-vc to get its column definitions, which
;; can't be autoloaded
(after-load 'ibuffer
  (require 'ibuffer-vc))

;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide) " "
              (size-h 9 -1 :right) " "
              (mode 16 16 :left :elide) " "
              filename-and-process)
        (mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide) " "
              (size-h 9 -1 :right) " "
              (mode 16 16 :left :elide) " "
              (vc-status 16 16 :left) " "
              filename-and-process)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)



;; Code folding

(use-package fold-dwim :ensure t)
(use-package fold-dwim-org :ensure t)

(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'fold-dwim-org/minor-mode)



;; navigation of ~everything (helm OR ivy)

(use-package flx :ensure t)
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-c h" . ivy-switch-buffer)
        ("C-c s" . swiper))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 15)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))

(use-package counsel :ensure t
  :bind*
  (("C-x f" . counsel-find-file)
   ("C-c i" . counsel-imenu)
   ("C-c a" . counsel-rg)
   ("C-c g s" . counsel-grep-or-swiper)
   ("C-c b" . counsel-descbinds)
   ("M-x" . counsel-M-x)))

(use-package swiper :ensure t)

;; jump to register (file shortcuts)
(set-register ?i (cons 'file "~/Dropbox/inbox.org"))
(set-register ?j (cons 'file "~/Dropbox/journal.org"))
(set-register ?l (cons 'file "~/Dropbox/logbook.org"))
(set-register ?c (cons 'file "~/code/dotfiles/.emacs.d/config"))

;; Windmove gives shift-up/down/left/right for window navigation
(windmove-default-keybindings)

(provide 'my-navigation)
;;; my-navigation.el ends here
