;;; my-languages.el --- Configuration for programming languages

;;; Commentary:

;;; Code:

;; Cider for interactive clojure programming
(use-package cider :ensure t)

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
(use-package markdown-mode :ensure t
  :config
  (add-auto-mode 'markdown-mode "\\.\\(md\\|markdown\\)\\'")
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))
(use-package pandoc-mode :ensure t)
(defun cd/yank-md ()
  "Yank a markdown link and enter a description for it."
  (interactive)
  (insert "[")
  (insert (read-from-minibuffer "Link text: "))
  (insert "](")
  (yank)
  (insert ")"))
(global-set-key (kbd "<f5>") 'cd/yank-md)


(use-package csv-mode :ensure csv-nav
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

(defun my-web-mode-hook ()
  "Hooks for Web mode.  Adjust indents"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

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
  :config (add-hook 'before-save-hook 'gofmt-before-save))

;; Rust
(use-package rust-mode :ensure t)
(use-package flymake-rust :ensure t)
(use-package flycheck-rust :ensure t)

(use-package company :ensure t
  :diminish company-mode
  :config (setq company-tooltip-align-annotations t)
  :bind (("TAB" . company-indent-or-complete-common)))

(use-package cargo :ensure t)

;; Python
(use-package virtualenvwrapper :ensure t
  :config
  (venv-initialize-interactive-shells) 
  (venv-initialize-eshell)
  (setq venv-location "/Users/davison/Envs/")
  (add-hook 'python-mode-hook (lambda () (venv-workon "num"))))

(use-package ob-ipython :ensure t)

(diminish 'hs-minor-mode)
(diminish 'smartparens-mode)


(provide 'my-languages)
;;; my-languages.el ends here
