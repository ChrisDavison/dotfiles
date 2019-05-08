;;; my-newstuff.el --- Stuff I'm trying out, which may make it's way into my full config.

;;; Commentary:

;;; Code:
;; =================================
;; ====== Magit - Git in Emacs =====
;; =================================
(use-package magit :ensure t)

;; =================================
;; ======== Elpy for Python ========
;; =================================
(use-package elpy :ensure t
  :config
  (add-hook 'python-mode-hook (lambda () (elpy-enable)))
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))

;; need to set up VENV usage
;; Python
(use-package virtualenvwrapper :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "/Users/davison/.envs/")
  (add-hook 'python-mode-hook (lambda () (venv-workon "ml"))))

;; =================================
;; ======== Rainbow brackets =======
;; =================================
;; Thought i detected a slowdown with this, so not auto-including
;; (use-package rainbow-delimiters
;;   :ensure t
;;   :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;; =================================
;; ========== Racer Config =========
;; =================================
;; Auto-completion for Rust

;; (use-package racer
;;   :ensure t
;;   :config
;;   (setq racer-cmd "/Users/davison/prog/z__NOT_MINE/racer/target/release/racer")
;;   (setq racer-rust-src-path "/Users/davison/prog/z__NOT_MINE/rust_1.3_src/src/")
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   (add-hook 'rust-mode-hook #'cargo-minor-mode)
;;   (add-hook 'racer-mode-hook #'company-mode))


;; =================================
;; ========== Latex Config =========
;; =================================
;; (require-package 'auctex)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq TeX-save-query nil)

;; (setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
;; (setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports

;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; (defun turn-on-outline-minor-mode ()
;;   (outline-minor-mode 1))

;; (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
;; (add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
;; (setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else

;; ;; Manage citations
;; (require 'tex-site)
;; (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
;; (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
;; (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
;; (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
;; (add-hook 'latex-mode-hook 'turn-on-reftex)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; (setq LaTeX-eqnarray-label "eq"
;;       LaTeX-equation-label "eq"
;;       LaTeX-figure-label "fig"
;;       LaTeX-table-label "tab"
;;       LaTeX-myChapter-label "chap"
;;       TeX-auto-save t
;;       TeX-newline-function 'reindent-then-newline-and-indent
;;       TeX-parse-self t
;;       TeX-style-path
;;       '("style/" "auto/"
;;         "/usr/share/emacs21/site-lisp/auctex/style/"
;;         "/var/lib/auctex/emacs21/"
;;         "/usr/local/share/emacs/site-lisp/auctex/style/")
;;       LaTeX-section-hook
;;       '(LaTeX-section-heading
;;         LaTeX-section-title
;;         LaTeX-section-toc
;;         LaTeX-section-section
;;         LaTeX-section-label))


;; =================================
;; ========== Helm config ==========
;; =================================
;; Switched to using ivy, however keeping this around whilst I get
;; used to Ivy.  Incase i want to switch back.

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

;; =================================
;; === Ace-mode - Jump to letter ===
;; =================================
;; ace-mode is fantastic.  It's a hybrid of ace-jump and isearch
;; Hybrid of isearch and ace-jump.  Type a single character in search and words 
;; beginning with that will highlight.  Press the highlighted letter to jump to 
;; that occurence
(use-package ace-isearch :ensure ace-jump-mode
  :diminish ""
  :config (global-ace-isearch-mode 1))

(defun ipython()
  (interactive)
  (ansi-term "/Users/davison/.envs/ml/bin/ipython" "ipython"))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i --pprint")

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun filepath ()
  (interactive)
  (message (buffer-file-name)))

(provide 'my-newstuff)
;;; my-newstuff.el ends here
