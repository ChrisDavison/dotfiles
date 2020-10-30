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
      cd/dark-theme 'kaolin-aurora)

(setq doom-font "Hack-12")
(setq doom-theme cd/dark-theme)


(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'auth-sources "~/.authinfo")
(add-hook! dired-mode 'dired-hide-details-mode)

(after! projectile
  (add-to-list 'projectile-project-root-files ".projectile-root"))

;;; Nov.el - read epubs in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;; Load my custom modules
(defun load!-with-message (filename)
  (load! filename)
  (message "Loaded config: %s" filename))

(load!-with-message "+rust")
;; (load!-with-message "+golang")
(load!-with-message "+bibcapture")
(load!-with-message "+fonts")
(load!-with-message "+misc")
(load!-with-message "+narrow")
(load!-with-message "+orgmode")
(load!-with-message "+vterm")
(load!-with-message "+ssh")
(load!-with-message "+keybinds")

(when is-wsl?
  (load!-with-message "+wsl-setup")
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
