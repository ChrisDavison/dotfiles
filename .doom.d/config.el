;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'f)
(require 's)
(require 'rx)
;;; General settings
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
      nov-text-width 80
      use-org-roam-on-startup nil
      org-roam-rename-file-on-title-change nil
      first-org-reload-done nil)

(setq ibuffer-formats
      `((mark modified read-only vc-status-mini " "
              (name 30 30 :left :elide) " "
              (mode 10 10 :left) " "
              vc-relative-file)))

(add-to-list 'auth-sources "~/.authinfo")
(add-hook! dired-mode 'dired-hide-details-mode)

(after! projectile
  (add-to-list 'projectile-project-root-files ".projectile-root"))

(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

;;; Nov.el - read epubs in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;; APPEARANCE (font and theme)
(defvar theme-preferences-light
  '(kaolin-breeze kaolin-light leuven apropospriate-light)
  "Light colour themes that I like")
(defvar theme-preferences-dark
  '(doom-one kaolin-bubblegum kaolin-eclipse kaolin-temple dracula)
  "Dark colour themes that I like")
(setq doom-font "Input-14")
(setq doom-variable-pitch-font "Montserrat-16")
(setq doom-theme (nth 3 theme-preferences-dark))
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

;;; GLOBAL MODES
(global-visual-line-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-undo-tree-mode 1)
(global-anzu-mode 1) ;; Live preview of search and replace (C-M-@)

;;; Hooks
(setq fill-column 120)
(add-hook 'org-mode-hook #'visual-fill-column-mode)
(add-hook 'org-mode-hook #'undo-tree-mode)
(add-hook 'org-mode-hook '(lambda () (interactive) (setq fill-column 120)))

(add-hook 'prog-mode-hook #'undo-tree-mode)
(setq lsp-lens-enable t)
(setq shell-file-name "/usr/bin/zsh")
;;; Programming - Rust
(add-hook! rust-mode
           '(company-mode
             flycheck-rust-setup
             cargo-minor-mode
             racer-mode
             (lambda () (add-to-list 'company-backends 'company-racer))))
(add-hook! racer-mode '(company-mode eldoc-mode))
(add-to-list 'auto-mode-alist '("\\.rs" . rust-mode))

;;; Programming - Golang
;; (add-to-list 'exec-path (concat (file-name-as-directory (getenv "GOPATH")) "bin") t)
;; (add-to-list 'load-path (concat (file-name-as-directory (getenv "GOPATH")) "src/github.com/dougm/goflymake"))
;; (require 'go-flymake)
                                        ; Use goimports instead of go-fmt for formatting with intelligent package addition/removal
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (go-eldoc-setup)
                                        ; call Gofmt before saving
                          (add-hook 'before-save-hook 'gofmt-before-save)))

;;; Programming - Python
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

;;; Programming - Haskell
(setq haskell-process-type 'stack-ghci)

;;; Programming - Common Lisp
(setq inferior-lisp-program (expand-file-name "~/code/z-external/ccl-dev/lx86cl64"))

(defvar cd/notes-dir (expand-file-name "~/code/knowledge/") "Where my notes are stored")
(defvar cd/journal-dir (f-join cd/notes-dir "journal/") "Where my journals are stored")
(defvar cd/logbook-dir (f-join cd/notes-dir "logbook/") "Where my logbook is stored")

;;; ORG MODE
(setq org-directory cd/notes-dir
      org-roam-directory org-directory)

(defun cd/org-mode-settings ()
  (interactive)
  (setq org-directory cd/notes-dir
        ;; Make 'd' the default priority, so priorities sort properly (ABC then D is blank)
        org-priority-default 68 ;; make 'D' the default priority, so ABC priorities sort
        org-priority-lowest 68 ;; make 'D' the lowest priority
        org-src-window-setup 'current-window
        org-indent-indentation-per-level 1
        org-adapt-indentation nil
        org-tags-column -120
        org-pretty-entities t
        org-catch-invisible-edits 'show-and-error
        org-imenu-depth 4
        org-link-frame-setup '((file . find-file-other-window))
        org-hide-emphasis-markers t
        org-todo-keywords '((sequence "TODO(t)" "MAYB(m)" "NEXT(n)" "WAIT(W)" "BLCK(b)" "WIP(w)" "|" "DONE(d)" "KILL(k)"))
        org-cycle-separator-lines 0
        org-list-indent-offset 2
        org-modules nil
        org-treat-insert-todo-heading-as-state-change t
        org-log-repeat nil
        org-log-done 'time
        org-log-done-with-time nil
        org-log-into-drawer t
        org-archive-location (format "%s::* From %%s" (f-join org-directory "archive.org"))
        org-refile-use-outline-path 't
        org-refile-allow-creating-parent-nodes 'confirm
        org-startup-folded 'fold
        org-id-track-globally t
        org-image-actual-width 600
        org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

;;; Org JOURNAL
  (setq org-journal-file-type 'daily
        org-journal-file-format "%Y/%Y-%m-%d-%A.org"
        org-journal-date-format "%F %A" ; e.g. 2021-01-01 Monday
        org-journal-time-format ""
        org-journal-dir cd/logbook-dir)

;;; Org DEFT
  (setq deft-directory org-directory
        deft-incremental-search nil
        deft-recursive t)

;;; Org ROAM
  (setq org-roam-directory org-directory
        +org-roam-open-buffer-on-find-file nil)

;;; Org CAPTURE
  ;; (load! "+literature_capture")
  (setq cd/capture-todos
        '("Todo"
         :keys "t"
         :template "* TODO %?"
         :file "work.org"
         :children (("Todo @personal" :keys "t" :file "todo.org")
                    ("Work @general" :keys "w" :headline "Tasks")
                    ("Work @glasdata" :keys "g" :headline "Tasks - IoF + GlasData")
                    ("Work @pitstop" :keys "p" :headline "Tasks - IoF + Pitstop")
                    ("Work @cybele" :keys "c" :headline "Tasks - Cybele"))))
  (setq cd/capture-gaming
        '("GAMING"
         :keys "g"
         :headline "Games to Buy"
         :template "* TODO %^{Game}"
         :immediate-finish t
         :children (("PC" :keys "p" :file "pc-gaming.org")
                    ("Nintendo Switch" :keys "n" :file "nintendo-switch.org"))))
  (setq cd/capture-journal
        '("Journal"
          :keys "j"
          :file (lambda () (f-join cd/journal-dir (format-time-string "journal-%Y.org")))
          :datetree t
          :children (("Journal note" :keys "j" :type item)
                     ("Journal entry" :keys "J" :type entry :template "* %?"))))
  (setq cd/capture-logbook
        '("Logbook"
          :keys "l"
          :file (lambda () (f-join cd/logbook-dir (format-time-string "logbook-%Y.org")))
          :datetree t
          :children (("Logbook note" :keys "l" :type item)
                     ("Logbook entry" :keys "L" :type entry :template "* %?"))))
  (setq cd/capture-work-project
        '("Current work PROJECT"
          :keys "p"
          :type item
          :file (lambda () (f-join "~/code/knowledge/logbook/tasks/" cd/current-work-project))
          :function find-todays-headline-or-create))
  (setq cd/capture-media
        '("MEDIA"
          :keys "m"
          :type entry
          :children (("Watch" :keys "w" :file "projects/stuff-to-watch.org"
                      :headline "REFILE" :template "* TODO %?")
                     ("Music" :keys "m" :file "projects/music.org"
                      :headline "REFILE"
                      :template "* TODO /\"%^{Title}\",/ by %^{Artist}"
                      :immediate-finish t))))
  (setq cd/capture-guitar-song
        '("Guitar song to learn"
          :keys "G"
          :file "projects/guitar.org" :headline "Songs to Learn"
          :immediate-finish t
          :template "* TODO %^{Artist} - /%^{Title}/"))
  (setq cd/capture-books
        '("Books / reading"
          :keys "b"
          :file "reading/reading.org" :headline "REFILE"
          :type item
          :template "%^{Book name}"))
  (setq cd/capture-something-to-anki
        '("Anki"
          :keys "a"
          :file "todo.org" :headline "anki"
          :immediate-finish t
          :template "* TODO %^{To Anki}"))
  (setq cd/capture-something-to-learn
        '("Learn"
          :keys "l"
          :file "todo.org" :headline "Stuff to learn (refile to appropriate file)"
          :immediate-finish t
          :template "* TODO learn about: %^{learn}"))
  (setq org-capture-templates
        (doct `(,cd/capture-journal ;; jj jJ
                ,cd/capture-logbook ;; ll lL
                ,cd/capture-work-project ;; p
                ,cd/capture-media ;; m
                ,cd/capture-guitar-song ;; G
                ,cd/capture-books ;; b
                ("Capture" :keys "c"
                 :file "projects/todo.org" :type entry :template "* TODO %?")
                ;; ,cd/capture-something-to-anki ;; a
                ;; ,cd/capture-something-to-learn ;; L
                ;; ,cd/capture-todos ;; t
                ;; ,cd/capture-gaming ;; g
                )))

;;; Org AGENDA
  (setq cd/agenda-oneday
        `("c1" "One day"
           ;; HIDE blocked or stuff I've put on hold (BLCK WAIT)
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-start-day "-0d")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("BLCK" "WAIT")))))
            ;; show a todo list of IN-PROGRESS
            (todo "WIP" ((org-agenda-overriding-header "Work - In Progress")
                         (org-agenda-files (org-files-work))
                         (org-agenda-todo-ignore-scheduled t)))
            ;; show a todo list of BLOCKED or WAITING
            (todo "WIP" ((org-agenda-overriding-header "In Progress")
                         (org-agenda-files (--filter (not (s-contains? "work.org" it)) (org-agenda-files)))
                         (org-agenda-sorting-strategy '((todo category-up todo-state-down priority-down)))
                         (org-agenda-todo-ignore-scheduled t))))))
  (setq cd/agenda-next-todos
        '("cn" "NEXT" ((todo "NEXT" nil))))
  (setq cd/agenda-review-last-week
        '("cr" "Review the last week"
           ((agenda "" ((org-agenda-start-day "-8d")
                        (org-agenda-entry-types '(:timestamp))
                        (org-agenda-archives-mode t)
                        (org-agenda-later 1)
                        (org-agenda_log-mode 16)
                        (org-agenda-show-log t))))))
  (setq cd/agenda-for-planning
        '("cp" "Planning"
           ;; kept as multiple todo commands so that grouping is done by todo state
           ;; rather than by category (which is my default todo sorting preference)
           ((todo "WIP" ((org-agenda-overriding-header "IN PROGRESS")
                         (org-agenda-tag-filter-preset '("-readinglist"))))
            (todo "NEXT" ((org-agenda-overriding-header "POSSIBLE NEXT TASKS")
                          (org-agenda-tag-filter-preset '("-readinglist"))))
            (todo "TODO" ((org-agenda-overriding-header "TODO")
                          (org-agenda-tag-filter-preset '("-readinglist"))))
            (todo "MAYB" ((org-agenda-overriding-header "UNSURE ABOUT")
                          (org-agenda-tag-filter-preset '("-readinglist")))))))
  (setq cd/agenda-work-only
        `("cw" "Work"
           ((todo "" ((org-agenda-files (quote ,(--filter (s-match "logbook" it) (org-agenda-files))))
                      (org-agenda-overriding-header "Work"))))))
  (setq cd/agenda-todos-no-books
        '("ct" "Todos, no books"
           ((todo "" ((org-agenda-tag-filter-preset '("-readinglist")))))))
  (setq cd/agenda-reading
        `("cR" "Reading -- in progress, and possible future books"
           ((todo "WIP|NEXT|MAYB|TODO"
                  ((org-agenda-files '(,(f-join org-directory "reading" "reading.org")))
                   (org-agenda-overriding-header "Books in Progress"))))))
  (setq cd/agenda-literature
        `("cL" "Literature in progress, and next options"
           ((todo "WIP|NEXT"
                  ((org-agenda-files '(,(f-join org-directory "literature.org")))
                   (org-agenda-overriding-header "Papers in Progress"))))))
  (setq org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-inhibit-startup nil
        org-agenda-files `(,(f-join cd/notes-dir "projects")
                           ,(f-join cd/logbook-dir "projects")
                           ,(f-join cd/logbook-dir "tasks")
                           ,(f-join cd/journal-dir (format-time-string "journal-%Y.org"))
                           ,(f-join cd/logbook-dir (format-time-string "logbook-%Y.org")))
        org-refile-targets `((org-agenda-files . (:maxlevel . 3)))
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
        org-agenda-skip-archived-trees nil
        org-agenda-block-separator ""
        org-agenda-compact-blocks nil
        org-agenda-todo-ignore-scheduled 'future
        org-agenda-sort-notime-is-late nil
        org-agenda-remove-tags t
        org-agenda-time-grid '((daily today require-timed remove-match)
                               (900 1000 1100 1200 1300 1400 1500 1600 1700)
                               "......"
                               "")
        org-agenda-use-time-grid nil
        org-agenda-scheduled-leaders '("" "LATE x%d")
        org-agenda-deadline-leaders '("" " in %dd")
        org-overriding-columns-format "%TODO %3PRIORITY %DEADLINE %40ITEM %TAGS"
        org-agenda-sorting-strategy '((agenda habit-up time-up category-up scheduled-up todo-state-up  priority-down)
                                      (todo priority-down category-up todo-state-down )
                                      (tags priority-down category-keep)
                                      (search category-keep))
        org-agenda-custom-commands `(("c" . "Custom agenda views")
                                     ,cd/agenda-oneday ;; c1
                                     ,cd/agenda-next-todos ;; cn
                                     ,cd/agenda-review-last-week ;; cr
                                     ,cd/agenda-for-planning ;; cp
                                     ,cd/agenda-work-only ;; cw
                                     ,cd/agenda-todos-no-books ;; ct
                                     ,cd/agenda-reading ;; cR
                                     ,cd/agenda-literature ;; cL
                                     ))

;;; Org-mode hooks
  (add-hook! org-mode
             'visual-line-mode
             'org-indent-mode
             'abbrev-mode
             'mixed-pitch-mode
             'undo-tree-mode
             '(lambda () (set-face-italic 'italic t)))
  (add-hook! 'auto-save-hook 'org-save-all-org-buffers))

(after! org (cd/org-mode-settings))

;;; Latex
;; (setq org-latex-default-packages-alist )
;;; SSH (remote server connections)
(setq my-remote-servers
      '(("skye" :username "cdavison" :ip "130.159.94.19")
        ("uist" :username "cdavison" :ip "130.159.95.176")
        ("bute" :username "cdavison" :ip "130.159.94.204")
        ("jura" :username "cdavison" :ip "130.159.94.214")
        ("iona" :username "cdavison" :ip "130.159.94.187")))

;;; WSL - Windows Subsystem for Linux
;; workaround to get the right WSL interop variable for clipboard usage
;; used in combination with a shell alias to export $WSL_INTEROP to a file
;; before calling emacs
(when (string-match ".*microsoft.*" (shell-command-to-string "uname -a"))
  (setenv "WSL_INTEROP" (string-trim (shell-command-to-string "cat ~/.wsl_interop")))
  (setq is-wsl? t
        browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args '("/c" "start")
        browse-url-browser-function #'browse-url-generic
        x-selection-timeout 10)

  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "win32yank.exe -i")
    (deactivate-mark))

  (defun wsl-paste ()
    (interactive)
    (let ((clipboard
           (shell-command-to-string "win32yank.exe -o")))
      (insert (substring (replace-regexp-in-string "\r" "" clipboard) 0 -1))))
  (message "WSL Interop setup"))

;;; Load external custom modules
;; (load! "+literature_capture")
(load! "+keybinds")
(load! "+functions") ;; also remember autoload.el

(setq-default org-download-method 'directory)
;; (setq-default org-download-image-dir (f-join org-directory "assets"))
(setq org-download-image-dir '(lambda () (interactive) (get-relative-asset-dir)))

;; (advice-add 'org-download-image :before
;;             #'(lambda (orig &rest args) (interactive)
;;                 (setq org-download-method 'directory
;;                       org-download-image-dir (get-relative-asset-dir))
;;                 (apply orig args)))
