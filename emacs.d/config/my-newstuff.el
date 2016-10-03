;;; my-newstuff.el --- Stuff I'm trying out, which may make it's way into my full config.

;;; Commentary:

;;; Code:
;; =================================
;; ====== Magit - Git in Emacs =====
;; =================================
(use-package magit :ensure t)

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


(provide 'my-newstuff)
;;; my-newstuff.el ends here
