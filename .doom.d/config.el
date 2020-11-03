;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(after! s
 (setq is-wsl? (s-contains? "microsoft" (shell-command-to-string "uname -a"))))

;;; General settings
(setq user-full-name "Chris Davison"
      user-mail-address "c.jr.davison@gmail.com"
      auto-save-default t
      auto-save-timeout 5
      avy-all-windows t
      vterm-shell "/usr/bin/fish"
      recentf-auto-cleanup 60
      global-auto-revert-mode t
      projectile-project-search-path '("~/code")
      display-line-numbers-type t
      +format-with-lsp nil
      nov-text-width 80
      cd/use-org-roam-on-startup nil
      cd/light-theme 'kaolin-breeze
      cd/dark-theme 'doom-dracula)

(setq doom-font "Hack-12")
(setq doom-theme cd/dark-theme)

(setq ibuffer-formats
      `((mark modified read-only vc-status-mini " "
              (name 30 30 :left :elide) " "
              (mode 10 10 :left) " "
              vc-relative-file)))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'auth-sources "~/.authinfo")
(add-hook! dired-mode 'dired-hide-details-mode)

(after! projectile
  (add-to-list 'projectile-project-root-files ".projectile-root"))

;;; Nov.el - read epubs in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;; Load my custom modules
(load! "+rust")
;; (load! "+golang")
(load! "+bibcapture")
(load! "+fonts")
(load! "+misc")
(load! "+narrow")
(load! "+orgmode")
(load! "+orgcapture")
(load! "+orgagenda")
(load! "+vterm")
(load! "+ssh")
(load! "+keybinds")

(when is-wsl?
  (load! "+wsl-setup")
  (setq x-selection-timeout 10))

;;; Final stuff (launch modes etc)
(global-visual-line-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-undo-tree-mode 1)
(global-anzu-mode 1) ;; Live preview of search and replace (C-M-@)

(if cd/use-org-roam-on-startup
    (org-roam-mode)
  (message "Org-roam mode not started automatically"))

