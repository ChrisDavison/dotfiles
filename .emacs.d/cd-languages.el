;;; cd-languages.el --- Configuration for programming languages

;;; Commentary:

;;; Code:

;; Cider for interactive clojure programming
(use-package flycheck-clojure :ensure t)

(use-package cider :ensure t
  :config
  (setq nrepl-popup-stacktraces nil)
  (after-load 'cider
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode)
    (add-hook 'cider-repl-mode-hook 'smartparens-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    (after-load 'clojure-mode
      (after-load 'flycheck
        (flycheck-clojure-setup)))))

(use-package flycheck :ensure t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-temp-prefix ".flycheck")
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; not sure if add-auto-mode works like the setq...so leaving for now
(defun cd/auto-fill-at-80 ()
  "Use auto fill mode and wrap at col 80."
  (progn
    (auto-fill-mode)
    (set-fill-column 80)))

(use-package markdown-mode :ensure t
  :config
  (add-auto-mode 'markdown-mode "\\.\\(md\\|markdown\\)\\'")
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  (add-hook 'markdown-mode-hook 'cd/auto-fill-at-80))

(use-package pandoc-mode :ensure t :diminish "")
(defun cd/yank-md ()
  "Yank a markdown link and enter a description for it."
  (interactive)
  (insert "[")
  (insert (read-from-minibuffer "Link text: "))
  (insert "](")
  (yank)
  (insert ")"))
(global-set-key (kbd "<f5>") 'cd/yank-md)


(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :config (setq csv-separators '("," ";" "|" " ")))

;; Lua
(use-package lua-mode :ensure t)

;; Emmet is fantastic for quickly outlining HTML
(use-package emmet-mode :ensure t
  :config 
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (setq emmet-move-cursor-between-quotes t))

(use-package haml-mode :ensure t)
(use-package sass-mode :ensure t)
(use-package scss-mode :ensure t
  :config (setq-default scss-compile-at-save t))

(use-package js2-mode :ensure t)
(use-package json-mode :ensure t)
(use-package coffee-mode :ensure t)
(use-package nodejs-repl :ensure t)
(use-package js-comint :ensure t
  :config
  (setq inferior-js-program-command "node")
  (add-hook 'js3-mode-hook
            '(lambda ()
               (local-set-key "\C-x\C-e" 'js-send-last-sexp)
               (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
               (local-set-key "\C-cb" 'js-send-buffer)
               (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
               (local-set-key "\C-cl" 'js-load-file-and-go))))

(setq inferior-js-mode-hook
      (lambda ()
        (ansi-color-for-comint-mode-on) ;; We like nice colors
        (add-to-list ;; Deal with some prompt nonsense
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))

(defun cd-web-mode-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (add-hook 'web-mode-hook 'cd-web-mode-hook))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-conten-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; Colourize CSS literals
(use-package rainbow-mode :ensure t
  :config 
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'sass-mode-hook 'rainbow-mode))

;; Go
(use-package go-mode :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports"))

;; Rust
(use-package rust-mode :ensure t)
(use-package flymake-rust :ensure t)
(use-package flycheck-rust :ensure t)

(use-package company :ensure t :diminish company-mode
  :bind ("TAB" . company-indent-or-complete-common)
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'python-mode 'company-mode)
  (add-hook 'org-mode 'company-mode))

(use-package cargo :ensure t)

;; Python
;; (use-package virtualenvwrapper :ensure t
;;   :config
;;   (venv-initialize-interactive-shells) 
;;   (venv-initialize-eshell)
;;   (setq venv-location "/Users/davison/Envs/")
;;   (add-hook 'python-mode-hook (lambda () (venv-workon "ml"))))

;; (use-package ob-ipython :ensure t)

(diminish 'hs-minor-mode)
(diminish 'smartparens-mode)

(defun cd-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun cd-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((cd-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(add-hook 'c-mode-common-hook 'cd-c-mode-common-hook)

(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

(yas-global-mode +1)

(use-package julia-mode :ensure t)
(use-package julia-repl :ensure t
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode))

(provide 'cd-languages)
;;; cd-languages.el ends here
