;;; cd-newstuff.el --- Stuff I'm trying out, which may make it's way into my full config.

;;; Commentary:

;;; Code:
;; =================================
;; ====== Magit - Git in Emacs =====
;; =================================
(use-package magit :ensure t
  :config
  (setq
   ;; Magit needs to call git multiple times
   ;; only refreshing the main buffer can improve performance
   magit-refresh-status-buffer nil
   ;; Emacs has its own version control.  We don't need to run both
   ;; as that'll be detrimental for performance
   vc-handled-backends (delq 'Git vc-handled-backends)))

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
;; (use-package auctex :ensure t
;;   :config
;;   (setq TeX-auto-save t
;;         TeX-parse-self t
;;         TeX-save-query nil
;;         ispell-program-name "aspell"
;;         ispell-dictionary "english")
;;   (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;   (add-hook 'LaTeX-mode-hook 'flyspell-buffer))

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
  :config
  (global-ace-isearch-mode 1)
  (setq ace-isearch-use-jump t)
  :bind (("C-c ." . ace-jump-mode)
         ("C-c j c" . ace-jump-char-mode)
         ("C-c j l" . ace-jump-line-mode)))

(defun ipython()
  (interactive)
  (if *is-windows*
      (progn (setq explicit-shell-file-name
                   "C:/python3/scripts/ipython.exe")
             (setq shell-file-name "ipython")
             (setq explicit-sh.exe-args '("--login" "-i"))
             (setenv "SHELL" shell-file-name)
             (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
             (shell)
             (cd/set-windows-shell))
    (ansi-term "/Users/davison/.envs/ml/bin/ipython" "ipython")))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i --pprint")

(defun what-face (pos)
  "Echo the face (font element) under point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun filepath ()
  "Echo the filepath as a message."
  (interactive)
  (message (buffer-file-name)))

(use-package saveplace :ensure t
  :config
  (setq save-place t
        save-place-file (concat user-emacs-directory "places")))

(setq uniquify-buffer-name-style 'forward)

(defun cd/set-windows-shell ()
  "If on windows, set the shell to git bash."
  (interactive)
  (if (eq system-type 'windows-nt)
      (progn (setq explicit-shell-file-name
                   "C:/Program Files/Git/bin/sh.exe")
             (setq shell-file-name "bash")
             (setq explicit-sh.exe-args '("--login" "-i"))
             (setenv "SHELL" shell-file-name)
             (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))
    nil))

(cd/set-windows-shell)
(global-set-key (kbd "C-c C-f") 'hs-toggle-hiding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           IMPROVEMENT FOR ANSI-TERM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Close an ansi-term buffer if I quit the terminal."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; By default, use fish in ansi-term
;; e.g. don't prompt for a shell
(defvar my-term-shell "/usr/local/bin/fish")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Use UTF8 in terminals
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unx 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

;; Make URLs in the term clickable
(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste))
(add-hook 'term-mode-hook 'my-term-hook)

(provide 'cd-newstuff)
;;; cd-newstuff.el ends here
