;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Chris Davison"
      user-mail-address "c.jr.davison@gmail.com")

;;; Doom appearance Utility
(setq doom-font "Dank Mono-14")
(setq doom-theme 'doom-one)
(setq display-line-numbers-type "relative")

(global-visual-line-mode 1)

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

;; (require! 'org-roam-protocol)
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

;; Much of the commented stuff is either stuff that I'm not sure about
;; or isn't actually different from the org-mode (or doom org) defaults.
(setq org-directory "~/Dropbox/org"
;;       org-default-notes-file "~/code/knowledge/inbox.org"
      org-src-window-setup 'current-window
      org-indent-indentation-per-level 1
      org-adapt-indentation nil
      org-pretty-entities t
      org-catch-invisible-edits 'show-and-error
      org-imenu-depth 4
;;       ;; Use M-+ M-- to change todo, and leave S-<arrow> for windows
;;       org-replace-disputed-keys t
      org-hide-emphasis-markers t
      org-todo-keywords'((sequence "TODO(t)" "WIP" "WAIT" "|" "DONE")
                         (sequence "|" "DEAD"))
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-cycle-separator-lines 0
      org-list-indent-offset 2
;;       org-modules '(org-bibtex org-habit org-tempo)
      org-agenda-files '("~/Dropbox/org/projects")
;;       org-agenda-time-grid '((daily today require-timed) (900 1300 1700 2100) "  --- " "")
;;       org-agenda-confirm-kill nil
      org-log-repeat nil
      org-log-done-with-time nil
;;       org-ellipsis "…"
      org-archive-location "~/Dropbox/org/archive.org::"
;;       ;; Settings for refiling
;;       org-reverse-note-order t
      org-refile-use-outline-path t
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
      org-agenda-skip-scheduled-if-deadline-is-shown t
     )
;; (setq org-roam-directory org-directory)

(defun read-capitalized-title ()
  (s-titleize (read-string "Title: ")))

(defun read-author ()
  (let ((name (read-string "Author: " "" nil nil)))
    (if (s-equals? name "")
        nil
      (format-author-name name))))

(defun format-author-name (author)
  (concat (seq-mapcat
           (lambda (author-part)
             (if (> (length author-part) 1)
                 (s-concat " " (s-capitalize author-part))
               (s-concat (s-capitalize author-part) ".")))
           (s-split " " author))))

(defun maybe-get-bibtex ()
  "Maybe get a DOI number for a reference"
  (let ((doi (read-string "DOI: " "" nil nil)))
    (if (s-equals? doi "")
        nil
      (s-concat ("\n")))))

(defun read-authors ()
  (setq authors (read-author)
        running t)
  (while running
    (setq input (read-author))
    (if (s-equals? input nil)
        (setq running nil)
      (setq authors (concat authors " and " input))))
  authors)

(setq org-capture-templates
      '(
        ("t" "Todo" entry (file "~/Dropbox/org/inbox.org") "* TODO %?")
        ("r" "Research" entry (file "~/Dropbox/org/inbox.org") "** TODO Research %?")

        ("n" "Note")
        ("nn" "List item" item (file+headline "~/Dropbox/org/inbox.org" "Notes")
         "- %?")
        ("nl" "List link" item (file+headline "~/Dropbox/org/inbox.org" "Notes")
         "- [[%^{URL}][%^{Description}]] %?")
        ("nN" "Entry" entry (file "~/Dropbox/org/inbox.org") "* %?")

        ("l" "Logbook")
        ("ll" "Logbook item" item (file+datetree "~/Dropbox/org/logbook.org")
         "- %?")
        ("lL" "Logbook entry" entry (file+datetree "~/Dropbox/org/logbook.org")
         "* %?")
        ("ld" "Logbook entry (dated)" entry (file+datetree "~/Dropbox/org/logbook.org")
         "* %?" :time-prompt t)

        ("g" "Games")
       ("gp" "PC" entry (file+olp "pc-games.org" "Future / Unreleased" "gaming.org" "PC")
         "* %^{Todo|TODO|WAIT|BUY|NEXT|PLAYING|DONE} %^{PC game}\n:%?")
        ("gn" "Nintendo Switch" entry (file+olp "nintendo-switch-games.org" "Future / Unreleased")
         "* %^{Todo|TODO|WAIT|BUY|NEXT|PLAYING|DONE} %^{Nintendo Switch game}\n:%?\n")
        ("gt" "Tabletop" entry (file+headline "tabletop-games.org" "Potential Purchases")
         "* %^{Todo|TODO|BUY} %^{Tabletop game}\n%?\n")
        ("w" "Watch")
        ("wt" "TV" item
         (file+olp "tv-shows-and-films.org" "TV Shows / Series" "To Watch")
         "%^{TV}" :immediate-finish t)
        ("wf" "film" item
         (file+olp "tv-shows-and-films.org" "Films" "To Watch")
         "%^{Film}" :immediate-finish t)

        ("L" "Literature" entry (file+headline "literature.org" "REFILE")
         "** TODO %(read-capitalized-title)\n\nAuthors: %(read-authors)\n\n#+BEGIN_SRC bibtex\n#+END_SRC" :immediate-finish t)

        ("b" "book" entry (file+olp "books.org" "Book List" "Refile")
         "** TO-READ %^{Book}\n%^{AUTHOR}p")

        ("c" "Calendar" entry (file+olp+datetree "calendar.org")
         "* TODO %?\nDEADLINE: %t" :time-prompt t)

        ("j" "journal")
        ("jj" "Journal Item" item (file+datetree "journal.org") "%?")
        ("jJ" "Journal Entry" entry (file+datetree "journal.org") "* %?")

        ("Q" "Quote" entry (file "quotes.org")
         "* %^{Quote Topic}\n#+BEGIN_QUOTE\n%^{Quote} (%^{Author})\n#+END_QUOTE")
        ))

(setq org-agenda-custom-commands
      '(
        ("1" "Today, no upcoming deadlines"
         ((agenda "" ((org-agenda-span 1)
                      (org-agenda-use-time-grid t)
                      (org-deadline-warning-days 0)))))
        ("7" "Week, no upcoming deadlines"
         ((agenda "" ((org-agenda-span 7)
                      (org-deadline-warning-days 0)))))))

(defun cd/refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))

(defun cd/refile-to-file (&optional target)
  (interactive)
  (let ((filename (or target (read-file-name "Refile to: ")))
        (old-refile-targets org-refile-targets))
    (progn (setq org-refile-targets `((filename . (:maxlevel . 6))))
           (org-refile)
           (setq org-refile-targets old-refile-targets))))

(defun cd/refile-to-this-file ()
  (interactive)
  (refile-to-file (buffer-name)))

(require 'org-element)

(defun org-file-from-subtree ()
  "Cut the subtree currently being edited and create a new file from it.

  If called with the universal argument, prompt for new filename,
  otherwise use the subtree title"
  (interactive "P")
  (let ((filename (expand-file-name (read-file-name "New file name:"))))
    (org-cut-subtree)
    (find-file-noselect filename)
    (with-temp-file filename
      (org-mode)
      (yank))
    (find-file filename)))
(define-key org-mode-map (kbd "C-x C-n") 'org-file-from-subtree)

(setq vterm-shell "/usr/bin/fish")

;; workaround to get the right WSL interop variable for clipboard usage
;; used in combination with a shell alias to export $WSL_INTEROP to a file
;; before calling emacs
(when (s-contains? (shell-command-to-string "uname -a") "microsoft")
  (set-wsl-interop))

(global-anzu-mode 1)

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
        :desc "List all managed repos" "l" #'cd/repo/list))
      (:prefix-map ("j" . "jump to register")
       :desc "config" "c" #'(lambda () (interactive) (jump-to-register ?c))
       :desc "packages" "p" #'(lambda () (interactive) (jump-to-register ?p))
       :desc "inbox" "i" #'(lambda () (interactive) (jump-to-register ?i))
       :desc "journal" "j" #'(lambda () (interactive) (jump-to-register ?j))
       :desc "logbook" "l" #'(lambda () (interactive) (jump-to-register ?l))))

(map! :n "C-;" #'iedit-mode)
(map! :n "C-:" #'iedit-mode-toggle-on-function)

(map! "<f1>" 'org-capture)
(map! "<f2>" 'org-agenda)



(after! org-roam
        (map! :leader
            :prefix "n"
            :desc "org-roam" "l" #'org-roam
            :desc "org-roam-insert" "i" #'org-roam-insert
            :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
            :desc "org-roam-find-file" "f" #'org-roam-find-file
            :desc "org-roam-show-graph" "g" #'org-roam-show-graph
            :desc "org-roam-insert" "i" #'org-roam-insert
            :desc "org-journal" "j" #'org-journal-new-entry
            :desc "org-roam-capture" "c" #'org-roam-capture))

(add-hook! 'after-init-hook 'org-roam-mode)

;; (require 'company-org-roam)
;; (use-package company-org-roam
;;   :when (featurep! :completion company)
;;   :after org-roam
;;   :config
;;   (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

;; (use-package! org-journal
;;       :custom
;;       (org-journal-dir "~/Dropbox/org/journal")
;;       (org-journal-date-prefix "#+TITLE: ")
;;       (org-journal-file-format "%Y-%m.org")
;;       (org-journal-date-format "%A, %d %B %Y"))
;; (setq org-journal-enable-agenda-integration t)

;;; Nov.el - read epubs in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 80)

;;; org-roam / deft / zetteldeft
(setq deft-directory org-directory
      deft-recursive t)
;; (after! org-roam
;;         (map! :leader
;;             :prefix "n"
;;             :desc "org-roam" "l" #'org-roam
;;             :desc "org-roam-insert" "i" #'org-roam-insert
;;             :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
;;             :desc "org-roam-find-file" "f" #'org-roam-find-file
;;             :desc "org-roam-show-graph" "g" #'org-roam-show-graph
;;             :desc "org-roam-insert" "i" #'org-roam-insert
;;             :desc "org-journal" "j" #'org-journal-new-entry
;;             :desc "org-roam-capture" "c" #'org-roam-capture))
;; (add-hook! 'after-init-hook 'org-roam-mode)

;; (require 'company-org-roam)
;; (use-package company-org-roam
;;   :when (featurep! :completion company)
;;   :after org-roam
;;   :config
;;   (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

;;; registers - easily navigate to files, or specific places
(set-register ?c '(file . "~/.doom.d/config.el"))
(set-register ?p '(file . "~/.doom.d/packages.el"))
(set-register ?i '(file . "~/Dropbox/org/inbox.org"))
(set-register ?j '(file . "~/Dropbox/org/journal.org"))
(set-register ?l '(file . "~/Dropbox/org/logbook.org"))
