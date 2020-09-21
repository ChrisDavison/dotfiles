;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Chris Davison"
      user-mail-address "c.jr.davison@gmail.com")

;;; Doom appearance Utility
(setq doom-font "Hack-14")
(setq doom-theme 'doom-nord)
(setq display-line-numbers-type t)

(global-visual-line-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Doom util functions - quick documentation
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; Programming - rust
(add-hook! rust-mode
           '(company-mode
             flycheck-rust-setup
             cargo-minor-mode
             racer-mode
             (lambda () (add-to-list 'company-backends 'company-racer))))
(add-hook! racer-mode '(company-mode eldoc-mode))
(add-to-list 'auto-mode-alist '("\\.rs" . rust-mode))

;;; General settings
(setq-default auto-save-default t)
(setq avy-all-windows t)

(delete-selection-mode 1)
(global-undo-tree-mode 1)

(setq fullscreen-at-startup nil)
(when fullscreen-at-startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))
(add-to-list 'auth-sources "~/.authinfo")

(setq vterm-shell "/usr/bin/fish")

(global-anzu-mode 1) ;; Live preview of search and replace (C-M-@)

(setq global-auto-revert-mode t)

(setq projectile-project-search-path '("~/code"))

;;; Easier connection to machines over ssh
(defvar remote-machines
  `(("skye" . ,(list :username "cdavison" :ip "130.159.94.19"))
    ("iona" . ,(list :username "cdavison" :ip "130.159.94.187"))))

(defun connect-remote ()
  "Open dired buffer in selected remote machine"
  (interactive)
  (let* ((machines (mapcar 'car remote-machines))
         (selected-machine (completing-read "Machine" machines nil t))
         (machine-data (cdr (assoc selected-machine remote-machines)))
         (username (plist-get machine-data :username))
         (ip-address (plist-get machine-data :ip)))
    (if (string= username "root")
        (dired (concat "/ssh:" username "@" ip-address ":/"))
      (dired (concat "/ssh:" username "@" ip-address ":/home/" username "/")))
    (message "Connected")))

(after! projectile
        (add-to-list 'projectile-project-root-files ".projectile-root"))

;;; Keybinds


;;; Nov.el - read epubs in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 80)

;;; registers - easily navigate to files, or specific places
(set-register ?c '(file . "~/Dropbox/org/projects/cybele.org"))
(set-register ?i '(file . "~/Dropbox/org/projects/iof2020.org"))
(set-register ?t '(file . "~/Dropbox/org/projects/todo.org"))
(set-register ?w '(file . "~/Dropbox/org/projects/work.org"))

(defun load!-with-message (filename)
  (load! filename)
  (message "Loading %s" filename))

(load!-with-message "+bibcapture")
(load!-with-message "+fonts")
(load!-with-message "+misc")
(load!-with-message "+narrow")
(load!-with-message "+org-config")
(load!-with-message "+org-capture")
(load!-with-message "+org-agenda")
(load!-with-message "+org-journal")
(load!-with-message "+wsl-setup")
(load!-with-message "+vterm")

(message "Org-roam startup can be slow...monitor it? Might be fine now I'm running emacs as daemon.")
(org-roam-mode)


(add-to-list 'exec-path (concat (file-name-as-directory (getenv "GOPATH")) "bin") t)
(add-to-list 'load-path (concat (file-name-as-directory (getenv "GOPATH")) "src/github.com/dougm/goflymake"))
(require 'go-flymake)
; Use goimports instead of go-fmt for formatting with intelligent package addition/removal
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (go-eldoc-setup)
                          ; call Gofmt before saving
                          (add-hook 'before-save-hook 'gofmt-before-save)))

(add-hook! dired-mode 'dired-hide-details-mode)

;;; Mappings
(map! "C-<" 'avy-goto-word-1) ;; C-S-,
(map! :leader "j" 'jump-to-register)

(map! :v "C-c C-c" 'wsl-copy)
(map! :v "C-c C-v" 'wsl-paste)

(map! :leader :prefix "w" :desc "evil-window-split (follow)"
      "s" (lambda () (interactive) (+evil-window-split-a) (evil-window-down 1)))
(map! :leader :prefix "w" :desc "evil-window-vsplit (follow)"
      "v" (lambda () (interactive) (+evil-window-vsplit-a) (evil-window-right 1)))

(map! :leader
      (:prefix-map ("a" . "applications")
       (:prefix ("r" . "repoutil")
        :desc "Status of all branches" "b" #'cd/repo/branchstat
        :desc "Fetch all branches" "f" #'cd/repo/fetch
        :desc "List all managed repos" "l" #'cd/repo/list)
       (:prefix ("a" . "agenda (custom)")
        :desc "Agenda" "a" '(lambda ()
                              (interactive)
                              (org-agenda "" "a")
                              (org-agenda-week-view))
        :desc "One day" "1" '(lambda () (interactive) (org-agenda "" "c1"))
        :desc "Media" "m" '(lambda () (interactive) (org-agenda "" "cm"))
        :desc "Work" "w" '(lambda () (interactive) (org-agenda "" "cw"))
        )
       (:prefix ("o" . "org (custom)")
        :desc "subtree to file" "s" 'cd/org-file-from-subtree
        )
       ))

;; Test editing
(map! :n "C-;" #'iedit-mode)
(map! :n "C-:" #'iedit-mode-toggle-on-function)
(map! "M-%" #'anzu-query-replace)
(map! "C-M-%" #'anzu-query-replace-regexp)

;; Emacs capture and org-mode
(map! "<f1>" 'org-capture)
(map! "<f2>" 'org-agenda)
(map! "<f3>" '(lambda () (interactive) (org-agenda nil "c1")))
(map! "<f4>" '(lambda () (interactive) (org-agenda nil "cW")))
(map! :map org-mode-map :n "<SPC> m r a" 'change-state-and-archive)
(map! :map org-mode-map :n "<SPC> m d i" 'org-time-stamp-inactive)
(map! :map dired-mode-map :n "/" 'dired-narrow)

(after! evil
  (map! :nv "j" 'evil-next-visual-line
        :nv "k" 'evil-previous-visual-line))

(setq recentf-auto-cleanup 60)
