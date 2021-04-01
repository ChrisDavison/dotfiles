;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'dash)
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

;;; APPEARANCE (font and theme)
(setq theme-preferences-light '(kaolin-breeze kaolin-light leuven apropospriate-light)
      theme-preferences-dark '(doom-one kaolin-bubblegum kaolin-eclipse kaolin-temple dracula))
(setq doom-font "Input-14")
(setq doom-variable-pitch-font "Montserrat-16")
(setq doom-theme (nth 3 theme-preferences-dark))
(setq fullscreen-at-startup t)
(when fullscreen-at-startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))
(setq cd-fonts (--filter (member it (font-family-list))
                         '("Input" "Dank Mono" "Hack" "Rec Mono Casual" "Rec Mono Linear" "Rec Mono SemiCasual"
                           "Inconsolata" "JetBrains Mono" "Source Code Pro" "Cascadia Code" "mononoki"
                           "Fantasque Sans Mono" "CamingoCode" "Roboto Mono" "Ubuntu Mono"
                           "Liberation Mono" "Fira Code" "Iosevka Term")))

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
               (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/jupyter")))))
(add-hook! 'pyvenv-post-deactivate-hooks
           '((lambda ()
               (setq python-shell-interpreter "python3"))))

(defun elpy-send-contiguous-block ()
  (interactive)
  (mark-paragraph)
  (elpy-shell-send-region-or-buffer)
  (evil-forward-paragraph))

(map! :map python-mode-map "C-c r" 'elpy-send-contiguous-block)

;;; Programming - Haskell
(setq haskell-process-type 'stack-ghci)

;;; Programming - Common Lisp
(setq inferior-lisp-program (expand-file-name "~/code/z-external/ccl-dev/lx86cl64"))

;;; ORG MODE
(setq org-directory (expand-file-name "~/code/knowledge/")
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
      ;; org-modules '(org-habit)
      org-modules nil
      org-treat-insert-todo-heading-as-state-change t
      org-log-repeat nil
      org-log-done 'time
      org-log-done-with-time nil
      org-log-into-drawer t
      org-archive-location (format "%s::* From %%s" (concat org-directory "archive.org"))
      org-refile-use-outline-path 't
      org-refile-allow-creating-parent-nodes 'confirm
      org-startup-folded 'fold
      org-id-track-globally t
      org-image-actual-width 600
      org-blank-before-new-entry '((heading . t) (plain-list-item . auto))

;;;; AGENDA
      org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-inhibit-startup nil
      org-agenda-files (--map (concat org-directory it)
                                 '("projects" "journal"))
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

;;;; JOURNAL
      org-journal-file-type 'daily
      org-journal-file-format "%Y/%Y-%m-%d-%A.org"
      org-journal-date-format "%F %A" ; e.g. 2021-01-01 Monday
      org-journal-time-format ""
      ;; org-journal-dir org-directory
      ;; cd/journal-dir (expand-file-name "~/code/knowledge/journal")
      cd/logbook-dir (expand-file-name "~/code/logbook")
      org-journal-dir cd/logbook-dir
;;;; DEFT
      deft-directory org-directory
      deft-incremental-search nil
      deft-recursive t
;;;; org-roam
      org-roam-directory org-directory
      +org-roam-open-buffer-on-find-file nil
 )

;;;; Org-capture
;; (load! "+literature_capture")
(setq org-capture-templates
      (doct
       `(
         ;; ("Todo"
         ;;  :keys "t"
         ;;  :template "* TODO %?"
         ;;  :file "work.org"
         ;;  :children (("Todo @personal" :keys "t" :file "todo.org")
         ;;             ("Work @general" :keys "w" :headline "Tasks")
         ;;             ("Work @glasdata" :keys "g" :headline "Tasks - IoF + GlasData")
         ;;             ("Work @pitstop" :keys "p" :headline "Tasks - IoF + Pitstop")
         ;;             ("Work @cybele" :keys "c" :headline "Tasks - Cybele")))

         ;; ("GAMING"
         ;;  :keys "g"
         ;;  :headline "Games to Buy"
         ;;  :template "* TODO %^{Game}"
         ;;  :immediate-finish t
         ;;  :children (("PC" :keys "p" :file "pc-gaming.org")
         ;;             ("Nintendo Switch" :keys "n" :file "nintendo-switch.org")
         ;;             ("Tabletop" :keys "t" :file "tabletop-games.org" :headline "
         ("Journal"
          :keys "j"
          :type item
          :file (lambda () (format-time-string "~/code/knowledge/journal/%Y/%Y-%m-%d-%A.org"))
          :function find-todays-headline-or-create
          :children (("Journal note" :keys "j" :type item)
                     ("Journal entry" :keys "J" :type entry)))
         ("Logbook"
          :keys "l"
          :file (lambda () (format-time-string "~/code/logbook/%Y/%Y-%m-%d-%A.org"))
          :function find-todays-headline-or-create
          :children (("Logbook note" :keys "l" :type item)
                     ("Logbook entry" :keys "L" :type entry)))
         ("Current work PROJECT"
          :keys "p"
          :type item
          :file (lambda () (f-join "~/code/logbook/tasks/" cd/current-work-project))
          :function find-todays-headline-or-create
          )

         ("MEDIA"
          :keys "m"
          :type entry
          :children (("Watch" :keys "w" :file "watch.org" :template "* TODO %?")
                     ("Music" :keys "m" :file "music.org"
                      :template "* TODO /\"%^{Title}\",/ by %^{Artist}"
                      :immediate-finish t)))

         ("Guitar song to learn"
          :keys "G"
          :file "guitar.org" :headline "Songs to Learn"
          :immediate-finish t
          :template "* TODO %^{Artist} -- %^{Title}")

         ("Books / reading"
          :keys "b"
          :file "reading.org" :headline "REFILE"
          :type item
          :template "%^{Book name}")

         ("Anki"
          :keys "a"
          :file "todo.org" :headline "anki"
          :immediate-finish t
          :template "* TODO %^{To Anki}")

        ("Learn"
          :keys "l"
          :file "todo.org" :headline "Stuff to learn (refile to appropriate file)"
          :immediate-finish t
          :template "* TODO learn about: %^{learn}")
         )))

;;;; Org-agenda
(defun org-files-work ()
  (--map (concat org-directory it)
         '("work.org" "literature.org")))

(defun org-files-non-work ()
  (cl-set-difference
   (org-agenda-files) (org-files-work)
   :test 'equal))

(setq org-agenda-custom-commands
      '(("c" . "Custom agenda views")

        ("cc" "'Clean' - today's agenda only"

         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-day "-0d")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("BLCK" "WAIT")))))))
         ;; today's agenda, with overdue
         ;; HIDE blocked or stuff I've put on hold (BLCK WAIT)
         ;; show a todo list of IN-PROGRESS
         ;; show a todo list of BLOCKED or WAITING
         ;; show a todo list of POSSIBLE-NEXT-ITEMS
        ("c1" "One day"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-day "-0d")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("BLCK" "WAIT")))))
          (todo "WIP" ((org-agenda-overriding-header "Work - In Progress")
                       (org-agenda-files (org-files-work))
                       (org-agenda-todo-ignore-scheduled t)))
          (todo "WIP" ((org-agenda-overriding-header "In Progress")
                       (org-agenda-files (--filter (not (s-contains? "work.org" it)) (org-agenda-files)))
                       (org-agenda-sorting-strategy '((todo category-up todo-state-down priority-down)))
                       (org-agenda-todo-ignore-scheduled t)))
          ))

        ("cn" "NEXT" ((todo "NEXT" nil)))

        ("cw" "Work" ((todo ""
                            ((org-agenda-files '((concat org-directory "work.org")))
                             (org-agenda-overriding-header "Work")))))

        ("ct" "Todos, no books"
         ((todo "" ((org-agenda-tag-filter-preset '("-readinglist"))))))

        ("cr" "Review the last week"
         ((agenda "" ((org-agenda-start-day "-8d")
                      (org-agenda-entry-types '(:timestamp))
                      (org-agenda-archives-mode t)
                      (org-agenda-later 1)
                      (org-agenda_log-mode 16)
                      (org-agenda-show-log t)))))

        ("cp" "Planning"
         ;; kept as multiple todo commands so that grouping is done by todo state
         ;; rather than by category (which is my default todo sorting preference)
         ((todo "WIP" ((org-agenda-overriding-header "IN PROGRESS")
                       (org-agenda-tag-filter-preset '("-readinglist"))))
          (todo "NEXT" ((org-agenda-overriding-header "POSSIBLE NEXT TASKS")
                        (org-agenda-tag-filter-preset '("-readinglist"))))
          (todo "TODO" ((org-agenda-overriding-header "TODO")
                        (org-agenda-tag-filter-preset '("-readinglist"))))
          (todo "MAYB" ((org-agenda-overriding-header "UNSURE ABOUT")
                        (org-agenda-tag-filter-preset '("-readinglist"))))))

        ("cR" "Reading -- in progress, and possible future books"
         ((todo "WIP|NEXT|MAYB|TODO"
                ((org-agenda-files '((concat org-directory "reading.org")))
                 (org-agenda-overriding-header "Books in Progress")))
          ))

        ("cL" "Literature in progress, and next options"
         ((todo "WIP|NEXT"
                ((org-agenda-files '((concat org-directory "literature.org")))
                 (org-agenda-overriding-header "Papers in Progress")))))

        ("ca" "Stuff to anki"
         ((todo "" ((org-agenda-regexp-filter-preset '("+anki"))
                    (org-agenda-sorting-strategy '((todo todo-state-down priority-down category-up))))))
         )))

;;;; Org-mode hooks
(add-hook! org-mode
           'visual-line-mode
           'org-indent-mode
           'abbrev-mode
           'mixed-pitch-mode
           'undo-tree-mode
           '(lambda () (set-face-italic 'italic t)))
(add-hook! 'auto-save-hook 'org-save-all-org-buffers)

;;; Latex
;; (setq org-latex-default-packages-alist )
;;; SSH (remote server connections)
(setq remote-machines
  '(("skye" :username "cdavison" :ip "130.159.94.19")
    ("uist" :username "cdavison" :ip "130.159.95.176")
    ("bute" :username "cdavison" :ip "130.159.94.204")
    ("jura" :username "cdavison" :ip "130.159.94.214")
    ("iona" :username "cdavison" :ip "130.159.94.187")))

(defun connect-remote ()
  "Open dired buffer in selected remote machine"
  (interactive)
  (let* ((machines (mapcar 'car remote-machines))
         (selected-machine (completing-read "Machine" machines nil t))
         (machine-data (cdr (assoc selected-machine remote-machines)))
         (username (plist-get machine-data :username))
         (ip-address (plist-get machine-data :ip)))
    (if (string= username "root")
        (dired (concat "/sshx:" username "@" ip-address ":/"))
      (dired (concat "/sshx:" username "@" ip-address ":/home/" username "/")))
    (message "Connected")))

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
