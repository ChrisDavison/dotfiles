;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(after! s
 (setq is-wsl? (s-contains? "microsoft" (shell-command-to-string "uname -a"))))

;;; General settings
(setq user-full-name "Chris Davison"
      user-mail-address "c.jr.davison@gmail.com"
      auto-save-default t
      auto-save-timeout 5
      avy-all-windows t
      vterm-shell "/usr/bin/zsh"
      recentf-auto-cleanup 60
      global-auto-revert-mode t
      projectile-project-search-path '("~/code")
      display-line-numbers-type t
      +format-with-lsp nil
      nov-text-width 80
      use-org-roam-on-startup nil
      first-org-reload-done nil
      theme-preferences-light '(kaolin-breeze kaolin-light leuven apropospriate-light)
      theme-preferences-dark '(doom-one kaolin-bubblegum kaolin-eclipse kaolin-temple dracula))

(setq doom-font "CamingoCode-14")
(setq doom-variable-pitch-font "Montserrat-16")
(setq doom-theme (nth 0 theme-preferences-dark))

(setq ibuffer-formats
      `((mark modified read-only vc-status-mini " "
              (name 30 30 :left :elide) " "
              (mode 10 10 :left) " "
              vc-relative-file)))

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'auth-sources "~/.authinfo")
(add-hook! dired-mode 'dired-hide-details-mode)

(after! projectile
  (add-to-list 'projectile-project-root-files ".projectile-root"))

;;; Nov.el - read epubs in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;; vterm configuration
(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))


;;; font configuration
(after! dash
  (setq cd-fonts (--filter (member it (font-family-list))
                           '("Dank Mono" "Hack" "Rec Mono Casual" "Rec Mono Linear" "Rec Mono SemiCasual"
                             "Inconsolata" "JetBrains Mono" "Source Code Pro" "Cascadia Code" "mononoki"
                             "Fantasque Sans Mono" "CamingoCode" "Roboto Mono" "Ubuntu Mono"
                             "Liberation Mono" "Fira Code" "Iosevka Term"))))

(defvar current-font-idx 0)

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

;;; Load my custom modules
(load! "+ssh")
(load! "+keybinds")
(load! "+orgmode")
(load! "+rust")
(load! "+functions") ;; also remember autoload.el
;; (load! "+golang")

(when is-wsl? (load! "+wsl-setup"))

;;; Final stuff (launch modes etc)
(global-visual-line-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-undo-tree-mode 1)
(global-anzu-mode 1) ;; Live preview of search and replace (C-M-@)

(add-hook 'after-init-hook #'reload-config)
(setq fill-column 120)
(add-hook 'org-mode-hook #'visual-fill-column-mode)
(add-hook 'org-mode-hook #'undo-tree-mode)
(add-hook 'org-mode-hook '(lambda () (interactive) (setq fill-column 120)))
(add-hook 'prog-mode-hook #'undo-tree-mode)

(setq fullscreen-at-startup t)
(when fullscreen-at-startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq shell-file-name "/usr/bin/zsh")
