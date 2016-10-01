;;; my-navigation.el --- Navigation and searching

;;; Commentary:
;;;
;;; From [[http://www.masteringemacs.org/articles/2011/03/25/working-multiple-files-dired/][HERE]]

;;; Code:

;; Generally improve dired appearance
(use-package dired+
  :config (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(use-package find-dired :ensure dired+
  :config
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
  (setq dired-omit-files "^\\.[^.]\\|\\.pdf$\\|\\.tex$") 
  (diredp-toggle-find-file-reuse-dir 1)) 


;; Hide files beginning with .[not a dot]

(setq dired-omit-mode t)
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.\\|^__.*$")


;; Rough version of TPope's vinegar

(defun cd/vinegar ()
  (interactive)
  (dired "."))

(global-set-key (kbd "C-c C--") 'cd/vinegar)

;; Show current and total matches while searching

;; Show current and total matches while searching
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap query-replace] . anzu-query-replace))
  :config (global-anzu-mode t))

;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; ace-mode is fantastic.  It's a hybrid of ace-jump and isearch
;; Hybrid of isearch and ace-jump.  Type a single character in search and words beginning with that will highlight.  Press the highlighted letter to jump to that occurence
(use-package ace-isearch :ensure t
  :config (global-ace-isearch-mode 1))

;; smartscan (Vim *)
;; From https://github.com/itsjeyd/emacs-config/blob/emacs24/init.el
;; This basically allows you to do something similar to VIM *...i.e. it'll jump forward or backward to the next occurence of the symbol under the cursor.
;; Bound to =M-n= and =M-b= by default, I think.
(use-package smartscan :ensure t
  :config (global-smartscan-mode t))


;; Prompt with a hud when switching to a window in a multi-pane display

;; Prompt with a hud when switching windows, if more than 2 windows
(use-package switch-window
  :ensure t
  :config (setq switch-window-shortcut-style 'alphabet)
  :bind ("C-x o" . switch-window))


;; Interactively modify the buffer list

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

(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy)
        ("C-c h" . ivy-switch-buffer)
        ("C-c s" . swiper)
        )
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 15)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))))

(use-package counsel :ensure t
  :bind*
  (("C-x f" . counsel-find-file)
   ("C-c i" . counsel-imenu)
   ("C-c a" . counsel-ag)
   ("C-c g s" . counsel-grep-or-swiper)
   ("C-c b" . counsel-descbinds)
   ("M-x" . counsel-M-x)))

(use-package swiper :ensure t)

;; ("C-c i" . helm-imenu)
;; ("C-c r" . helm-regexp)
;; ("M-y" . helm-show-kill-ring)

;; (use-package helm
;;   :diminish helm-mode
;;   :ensure t
;;   :init (require 'helm-config)
;;   :config 
;;   (setq helm-candidate-number-limit 100
;;         helm-idle-delay 0.0
;;         helm-input-idle-delay 0.01
;;         helm-quick-update t
;;         helm-M-x-requires-pattern nil
;;         helm-ff-skip-boring-files t
;;         helm-recentf-fuzzy-match t
;;         helm-buffers-fuzzy-matching t
;;         helm-locate-fuzzy-match t
;;         helm-imenu-fuzzy-match t
;;         helm-M-x-fuzzy-match t
;;         helm-semantic-fuzzy-match t
;;         helm-apropos-fuzzy-match t
;;         helm-split-window-in-side-p t)
;;   (helm-mode 1)
;;   (helm-autoresize-mode nil)
;;   :bind (("C-c h" . helm-mini)
;;          ("C-c a" . helm-apropos)
;;          ("C-c o" . helm-occur)
;;          ("C-c s" . helm-swoop)
;;          ("C-c i" . helm-imenu)
;;          ("C-c r" . helm-regexp)
;;          ("C-c g" . helm-do-grep)
;;          ("C-x C-f" . helm-find-files)
;;          ("M-y" . helm-show-kill-ring)
;;          ("M-x" . helm-M-x)))

;; (use-package helm-descbinds
;;   :ensure t
;;   :bind (("C-c b" . helm-descbinds)))
;; #+END_SRC


;; jump to register (file shortcuts)

(set-register ?i (cons 'file "~/Dropbox/notes/inbox.md"))
(set-register ?m (cons 'file "~/Dropbox/notes/ml.md"))
(set-register ?g (cons 'file "~/Dropbox/notes/guitar.md"))
(set-register ?l (cons 'file "~/Dropbox/notes/engd/logbook2016.md"))
(set-register ?t (cons 'file "~/Dropbox/notes/todo.md"))

(provide 'my-navigation)
;;; my-navigation.el ends here
