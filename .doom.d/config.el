;; -----------------------------------------------------------------------------
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; -----------------------------------------------------------------------------

(require 'dash)
(require 'f)
(require 's)
(require 'rx)

;; -----------------------------------------------------------------------------
;;; General settings
;; -----------------------------------------------------------------------------
(setq user-full-name "Chris Davison"
      user-mail-address "c.jr.davison@gmail.com"
      auto-save-default t
      auto-save-timeout 5
      avy-all-windows t
      vterm-shell "/usr/bin/zsh"
      recentf-auto-cleanup 60
      global-auto-revert-mode t
      projectile-project-search-path `(,(expand-file-name "~/code"))
      display-line-numbers-type t
      +format-with-lsp nil
      nov-text-width 80)
(setq-default org-roam-directory "~/code/knowledge")

(setq ibuffer-formats
      `((mark modified read-only vc-status-mini " "
              (name 30 30 :left :elide) " "
              (mode 10 10 :left) " "
              vc-relative-file)))

(add-to-list 'auth-sources "~/.authinfo")
(add-hook! dired-mode 'dired-hide-details-mode)

(after! projectile
  (add-to-list 'projectile-project-root-files ".projectile-root"))

;; Nov.el - read epubs in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'auto-mode-alist '("\\.scratch\\'" . org-mode))

;; -----------------------------------------------------------------------------
;;; APPEARANCE (font and theme)
;; -----------------------------------------------------------------------------
(defvar theme-preferences-light
  '(kaolin-breeze kaolin-light leuven apropospriate-light)
  "Light colour themes that I like")
(defvar theme-preferences-dark
  '(doom-one kaolin-bubblegum kaolin-eclipse kaolin-temple dracula)
  "Dark colour themes that I like")
(setq doom-theme (nth 0 theme-preferences-light))

(setq doom-font "CamingoCode-14")
(setq doom-variable-pitch-font "Montserrat-14")

(defvar fullscreen-at-startup t "Should emacs fullscreen when launched")
(when fullscreen-at-startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))
(defvar cd-fonts
  (--filter (member it (font-family-list))
            '("Input" "Dank Mono" "Hack" "Rec Mono Casual" "Rec Mono Linear" "Rec Mono SemiCasual"
              "Inconsolata" "JetBrains Mono" "Source Code Pro" "Cascadia Code" "mononoki"
              "Fantasque Sans Mono" "CamingoCode" "Roboto Mono" "Ubuntu Mono"
              "Liberation Mono" "Fira Code" "Iosevka Term"))
  "Fonts that I like, filtered to only ones installed locally.")

(defvar current-font-idx 0 "Which of cd-fonts is currently active")

(defun set-pretty-font ()
  "Set a font from one of the available fonts that I like"
  (interactive)
  (setq doom-font (ivy-read "Pick font:" cd-fonts))
  (doom/reload-font))

(defun next-font ()
  (interactive)
  (setq current-font-idx
        (% (+ 1 current-font-idx)
           (length cd-fonts)))
  (let ((next-font-name (nth current-font-idx cd-fonts)))
    (set-frame-font next-font-name 1)
    (message next-font-name)))

;; -----------------------------------------------------------------------------
;;; GLOBAL MODES
;; -----------------------------------------------------------------------------
(global-visual-line-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-undo-tree-mode 1)
(global-anzu-mode 1) ;; Live preview of search and replace (C-M-@)

;; -----------------------------------------------------------------------------
;;; Hooks
;; -----------------------------------------------------------------------------
(setq fill-column 120)

(add-hook 'prog-mode-hook #'undo-tree-mode)
(add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
(setq lsp-lens-enable t)
(setq shell-file-name "/usr/bin/zsh")

;; -----------------------------------------------------------------------------
;;; Programming - Rust
;; -----------------------------------------------------------------------------
(add-hook! rust-mode
           '(company-mode
             flycheck-rust-setup
             cargo-minor-mode
             racer-mode
             ))
(add-hook! racer-mode '(company-mode eldoc-mode))
(add-to-list 'auto-mode-alist '("\\.rs" . rust-mode))

;; -----------------------------------------------------------------------------
;;; Programming - Golang
;; -----------------------------------------------------------------------------
;; (add-to-list 'exec-path (concat (file-name-as-directory (getenv "GOPATH")) "bin") t)
;; (add-to-list 'load-path (concat (file-name-as-directory (getenv "GOPATH")) "src/github.com/dougm/goflymake"))
;; (require 'go-flymake)
                                        ; Use goimports instead of go-fmt for formatting with intelligent package addition/removal
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook
          '(lambda ()
             (set (make-local-variable 'company-backends) '(company-go))
             (local-set-key (kbd "M-.") 'godef-jump)
             (go-eldoc-setup)
                                        ; call Gofmt before saving
             (add-hook 'before-save-hook 'gofmt-before-save)))

;; -----------------------------------------------------------------------------
;;; Programming - Python
;; -----------------------------------------------------------------------------
(setq python-environment-directory "~/.envs/py"
      python-shell-interpreter-args "console --simple-prompt"
      elpy-rpc-python-command "~/.envs/py/bin/python")

(add-hook! 'pyvenv-post-activate-hooks
           '((lambda ()
               (setq python-shell-interpreter (f-join pyvenv-virtual-env "bin/jupyter")))))
(add-hook! 'pyvenv-post-deactivate-hooks
           '((lambda ()
               (setq python-shell-interpreter "python3"))))

(map! :map python-mode-map "C-c r" 'elpy-send-contiguous-block)

(setq lsp-imenu-index-symbol-kinds
  '(Class Method Property Field Constructor Enum Interface Function Struct Namespace))

;; -----------------------------------------------------------------------------
;;; Programming - Haskell
;; -----------------------------------------------------------------------------
(setq haskell-process-type 'stack-ghci)

;; -----------------------------------------------------------------------------
;;; Programming - Common Lisp
;; -----------------------------------------------------------------------------
(setq inferior-lisp-program (expand-file-name "~/code/z-external/ccl-dev/lx86cl64"))

;; -----------------------------------------------------------------------------
;;; SSH (remote server connections)
;; -----------------------------------------------------------------------------
(setq tramp-default-method "sshx")
(setq my-remote-servers
      '(("skye" :username "cdavison" :ip "130.159.94.19")
        ("uist" :username "cdavison" :ip "130.159.95.176" :hop "skye")
        ("bute" :username "cdavison" :ip "130.159.94.204" :hop "skye")
        ("jura" :username "cdavison" :ip "130.159.94.214" :hop "skye")
        ("iona" :username "cdavison" :ip "130.159.94.187" :hop "skye")))



;; -----------------------------------------------------------------------------
;;; Load external custom modules
;; -----------------------------------------------------------------------------
(load! "+keybinds")
(load! "+functions") ;; also remember autoload.el
(load! "+wsl")
(load! "+org")
