;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Chris Davison"
      user-mail-address "c.jr.davison@gmail.com")

;;; Doom appearance Utility
(setq doom-font "Dank Mono-14")
(setq doom-theme 'doom-snazzy)
(setq display-line-numbers-type "relative")

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
           '(company-mode flycheck-rust-setup
                          cargo-minor-mode
                          racer-mode
                          (lambda () (add-to-list 'company-backends 'company-racer))))
(add-hook! racer-mode '(company-mode eldoc-mode))
(add-to-list 'auto-mode-alist '("\\.rs" . rust-mode))

;;; General settings
(setq-default auto-save-default t)
(setq avy-all-windows t)

(display-time-mode 1)
(delete-selection-mode 1)
(global-undo-tree-mode 1)

(setq fullscreen-at-startup t)
(when fullscreen-at-startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))
(add-to-list 'auth-sources "~/.authinfo")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "~/bin/firefox")
;;; Org-mode
(defun cd/org-open-link-same ()
  (interactive)
  (let ((old-setup org-link-frame-setup))
    (setq org-link-frame-setup '((file . find-file)))
    (org-open-at-point)
    (setq org-link-frame-setup old-setup)))

(add-hook! 'org-mode
           'visual-line-mode
           'org-indent-mode
           'abbrev-mode
           ;; 'org-roam-mode
           '(lambda () (set-face-italic 'italic t)))



(setq vterm-shell "/usr/bin/fish")

;; workaround to get the right WSL interop variable for clipboard usage
;; used in combination with a shell alias to export $WSL_INTEROP to a file
;; before calling emacs
(after! s
  (when (s-contains? (shell-command-to-string "uname -a") "microsoft")
    (set-wsl-interop)
    (setq x-selection-timeout 10)))


(global-anzu-mode 1)

(setq global-auto-revert-mode t)

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
(map! "C-c f" 'next-font)
(map! "M-%" #'anzu-query-replace)
(map! "C-M-%" #'anzu-query-replace-regexp)

(map! :v "C-c C-c" 'wsl-copy)
(map! :v "C-c C-v" 'wsl-paste)

(map! :leader
      :prefix "w"
      :desc "evil-window-split (follow)"
      "s"
      (lambda () (interactive)
        (evil-window-split)
        (evil-window-down 1)))
(map! :leader
      :prefix "w"
      :desc "evil-window-vsplit (follow)"
      "v"
      (lambda () (interactive)
        (evil-window-vsplit)
        (evil-window-right 1)))

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
        ))
      (:prefix-map ("j" . "jump to register")
       :desc "config" "c" #'(lambda () (interactive) (jump-to-register ?c))
       :desc "packages" "p" #'(lambda () (interactive) (jump-to-register ?p))
       :desc "todo" "t" #'(lambda () (interactive) (jump-to-register ?t))
       :desc "journal" "j" #'(lambda () (interactive) (jump-to-register ?j))
       :desc "quotes" "q" #'(lambda () (interactive) (jump-to-register ?q))
       :desc "logbook" "l" #'(lambda () (interactive) (jump-to-register ?l))))

(map! :n "C-;" #'iedit-mode)
(map! :n "C-:" #'iedit-mode-toggle-on-function)

(map! "<f1>" 'org-capture)
(map! "<f2>" 'org-agenda)

;;; Nov.el - read epubs in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 80)

;;; registers - easily navigate to files, or specific places
(set-register ?c '(file . "~/.doom.d/config.el"))
(set-register ?p '(file . "~/.doom.d/packages.el"))
(set-register ?j '(file . "~/Dropbox/org/journal.org"))
(set-register ?t '(file . "~/Dropbox/org/projects/todo.org"))
(set-register ?l '(file . "~/Dropbox/org/projects/work.org"))
(set-register ?q '(file . "~/Dropbox/org/quotes.org"))

(setq cd-config-files
      '("+bibcapture"
        "+fonts"
        "+misc"
        "+narrow"
        "+org-config"
        "+org-capture"
        "+org-agenda"
        "+vterm"))

(defun cd/load-personal-config ()
  (interactive)
  (dolist (a cd-config-files)
    (message "Loading %s" a)
    (load! a)))

(cd/load-personal-config)

(message "Org-roam isn't loaded at startup as it seems very slow....debug it?")
;; (org-roam-mode)
