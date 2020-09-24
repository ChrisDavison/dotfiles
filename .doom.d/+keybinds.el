;;; ../code/dotfiles/.doom.d/+keybinds.el -*- lexical-binding: t; -*-

(map! "C-<" 'avy-goto-word-1) ;; C-S-,
(map! :leader "j" 'jump-to-register)

(map! :v "C-c C-c" 'wsl-copy)
(map! :v "C-c C-v" 'wsl-paste)

(map! :leader
      (:prefix-map ("a" . "applications")
       (:prefix ("r" . "repoutil")
        :desc "Status of all branches" "b" #'cd/repo/branchstat
        :desc "Fetch all branches" "f" #'cd/repo/fetch
        :desc "List all managed repos" "l" #'cd/repo/list)
       (:prefix ("j" . "journal")
        :desc "Search all entries" "s" #'org-journal-search-forever
        :desc "Open today" "o" #'org-journal-open-current-journal-file)))

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
(map! :map org-mode-map :n "<SPC> o s" 'cd/org-open-link-same)
(map! :map org-mode-map :n "<SPC> o o" 'org-open-at-point)
(map! :map org-mode-map :n "<SPC> N" 'org-toggle-narrow-to-subtree)
(map! :map org-mode-map
      :n "C-x C-n" 'cd/org-file-from-subtree
      :v "C-x C-n" 'cd/org-file-from-selection)

(map! "<f5>" #'(lambda () (interactive) (find-next-file nil)))
(map! "<f6>" #'(lambda () (interactive) (find-next-file t)))

(map! :map dired-mode-map :n "/" 'dired-narrow)

(map! :nv "j" 'evil-next-visual-line
      :nv "k" 'evil-previous-visual-line)
(map! :leader :prefix "w" :desc "evil-window-split (follow)"
      "s" (lambda () (interactive) (+evil-window-split-a) (evil-window-down 1)))
(map! :leader :prefix "w" :desc "evil-window-vsplit (follow)"
      "v" (lambda () (interactive) (+evil-window-vsplit-a) (evil-window-right 1)))

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
