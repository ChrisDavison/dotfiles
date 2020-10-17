;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Chris Davison"
      user-mail-address "c.jr.davison@gmail.com")

;;; Doom appearance Utility
(setq doom-font "Rec Mono SemiCasual-12")
(setq cd/light-theme 'kaolin-breeze)
(setq cd/dark-theme 'kaolin-aurora)
(setq doom-theme cd/dark-theme)

;;; Programming - rust
(add-hook! rust-mode
           '(company-mode
             flycheck-rust-setup
             cargo-minor-mode
             racer-mode
             (lambda () (add-to-list 'company-backends 'company-racer))))
(add-hook! racer-mode '(company-mode eldoc-mode))
(add-to-list 'auto-mode-alist '("\\.rs" . rust-mode))

;;; Programming - golang
;; (add-to-list 'exec-path (concat (file-name-as-directory (getenv "GOPATH")) "bin") t)
;; (add-to-list 'load-path (concat (file-name-as-directory (getenv "GOPATH")) "src/github.com/dougm/goflymake"))
;; (require 'go-flymake)
;;                                         ; Use goimports instead of go-fmt for formatting with intelligent package addition/removal
;; (setq gofmt-command "goimports")
;; (add-hook 'go-mode-hook (lambda ()
;;                           (set (make-local-variable 'company-backends) '(company-go))
;;                           (local-set-key (kbd "M-.") 'godef-jump)
;;                           (go-eldoc-setup)
;;                                         ; call Gofmt before saving
;;                           (add-hook 'before-save-hook 'gofmt-before-save)))

;;; General settings
(setq auto-save-default t
      avy-all-windows t
      vterm-shell "/usr/bin/fish"
      recentf-auto-cleanup 60
      global-auto-revert-mode t
      projectile-project-search-path '("~/code")
      display-line-numbers-type t
      +format-with-lsp nil
      )

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'auth-sources "~/.authinfo")
(add-hook! dired-mode 'dired-hide-details-mode)

(after! projectile
  (add-to-list 'projectile-project-root-files ".projectile-root"))

;;; Nov.el - read epubs in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 80)

;;; registers - easily navigate to files, or specific places
(set-register ?c '(file . "~/Dropbox/org/projects/cybele.org"))
(set-register ?i '(file . "~/Dropbox/org/projects/iof2020.org"))
(set-register ?t '(file . "~/Dropbox/org/projects/todo.org"))
(set-register ?w '(file . "~/Dropbox/org/projects/work.org"))
(set-register ?r '(file . "~/Dropbox/org/projects/reading.org"))
(set-register ?j '(file . "~/Dropbox/org/journal.org"))
(set-register ?m '(file . "~/Dropbox/org/projects/media.org"))

;;; Load my custom modules
(defun load!-with-message (filename)
  (load! filename)
  (message "Loading %s" filename))

(load!-with-message "+bibcapture")
(load!-with-message "+fonts")
(load!-with-message "+misc")
(load!-with-message "+narrow")
(load!-with-message "+orgmode")
(load!-with-message "+wsl-setup")
(load!-with-message "+vterm")
(load!-with-message "+ssh")
(load!-with-message "+keybinds")

;;; Final stuff (launch modes etc)
(global-visual-line-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-undo-tree-mode 1)
(global-anzu-mode 1) ;; Live preview of search and replace (C-M-@)
(org-roam-mode)

(add-hook! 'prog-mode-hook 'undo-tree-mode)
