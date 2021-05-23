;;; ../code/dotfiles/.doom.d/+keybinds.el -*- lexical-binding: t; -*-

(map! "C-<" 'avy-goto-word-1) ;; C-S-,

(map! :leader
      :desc "imenu sidebar" "s I" 'imenu-list-smart-toggle)
;; (map! :leader "j" 'jump-to-register)

(map! :leader
      :desc "<<here>>" "j h" 'jump-to-here-anchor
      :desc "todos" "j t" '(lambda () (interactive) (find-file "~/code/knowledge/todo.org"))
      :desc "work" "j w" '(lambda () (interactive) (find-file "~/code/knowledge/work/work.org"))
      :desc "last capture" "j c" '(lambda () (interactive) (org-capture-goto-last-stored))
      :desc "bookmarks" "j b" '(lambda () (interactive) (org-capture-goto-target "u"))
      :desc "logbook" "j l" '(lambda () (interactive) (org-capture-goto-target "l"))
      :desc "scratch" "j s" '(lambda () (interactive) (find-file "~/code/scratch/scratch.org")))

(map! :v
      "C-c C-c" 'wsl-copy
      "C-c C-v" 'wsl-paste)

(map! :leader
      (:prefix-map ("a" . "applications")
       (:prefix ("r" . "repoutil")
        :desc "Status of all branches" "b" #'repoutil-branchstat
        :desc "Fetch all branches" "f" #'repoutil-fetch
        :desc "List all managed repos" "l" #'repoutil-list
        :desc "List all unclean repos" "u" #'repoutil-unclean)
       (:prefix ("g" . "ripgrep")
        :desc "org notes" "o" 'rg-org
        :desc "journal" "j" 'rg-journal
        :desc "logbook" "l" 'rg-logbook)
       (:prefix ("d" . "downloader")
        :desc "quick add" "q" 'nas/quick-add-download
        :desc "list" "l" 'nas/list-downloads)
       ("n" 'new-in-git)
       ))

;; Text editing
(map! :n "C-;" #'iedit-mode
      :n "C-:" #'iedit-mode-toggle-on-function)

(map! "M-%" #'anzu-query-replace
      "C-M-%" #'anzu-query-replace-regexp)

;; Emacs capture and org-mode
(map! "<f1>" 'org-capture
      "<f2>" 'org-agenda
      "<f3>" '(lambda () (interactive) (org-agenda nil "co") (goto-char (point-min)))
      "<f4>" '(lambda () (interactive) (org-agenda nil "cr") (goto-char (point-min)))
      "<f5>" #'find-next-file
      "<f6>" #'find-previous-file
      "<f7>" #'org-roam-capture)

(map! :map org-mode-map :leader :n
      "m r a" 'org-change-state-and-archive
      "m r A" 'org-archive-to-archive-sibling
      "m r A" 'org-archive-to-archive-sibling
      "m r t" 'org-refile-to-this-file
      "m r T" 'org-refile-to-this-file-level1
      "m r F" 'cd/refile-to-top-level
      "m d i" 'org-time-stamp-inactive
      "m F" #'(lambda () (interactive) (let ((org-footnote-section nil)) (org-footnote-new)))
      "m h" #'headercount
      "o s" 'org-open-link-same-window
      "o o" 'org-open-at-point
      "o S" 'org-sidebar-toggle
      "Q" 'org-unfill-paragraph
      "N" 'org-toggle-narrow-to-subtree
      "m l u" 'org-copy-link-url)

(map! :map org-mode-map :n
      "C-x C-n" 'org-file-from-subtree
      :v "C-x C-n" 'org-file-from-selection
      "<f7>" 'move-to-next-narrow
      "<f8>" 'move-to-previous-narrow)

;; (map! :map rust-mode-map :leader :n "c d" 'racer-find-definition)

(map! :map dired-mode-map :n "/" 'dired-narrow)

(map! :nv "j" 'evil-next-visual-line
      :nv "k" 'evil-previous-visual-line)

(map! :leader
      :prefix "w"
      :desc "evil-window-split (follow)" "s"
      (lambda () (interactive) (evil-window-split) (evil-window-down 1))
      :desc "evil-window-vsplit (follow)" "v"
      (lambda () (interactive) (evil-window-vsplit) (evil-window-right 1)))

(map! "C-<left>" 'find-previous-file
      "C-<right>" 'find-next-file)

(map! :after projectile
      :leader
      :desc "Find Org-dir note" "<SPC>"
      ;; #'(lambda () (interactive) (counsel-file-jump nil org-directory))
      #'org-roam-find-file
      :desc "Find Org-dir project" "S-<SPC>"
      #'(lambda () (interactive) (org-roam-find-file "@project "))
      :desc "Find Org-dir WORK project" "M-<SPC>"
      ;; #'(lambda () (interactive) (counsel-file-jump nil org-directory))
      #'(lambda () (interactive) (org-roam-find-file "@work "))
      )

(map! :map haskell-mode-map
      "C-x C-e" 'haskell-process-load-file)
