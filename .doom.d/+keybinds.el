;;; ../code/dotfiles/.doom.d/+keybinds.el -*- lexical-binding: t; -*-

(map! "C-<" 'avy-goto-word-1) ;; C-S-,
;; (map! :leader "j" 'jump-to-register)
(map! :leader
      :desc "jump to here anchor" "j h" 'jump-to-here-anchor
      :desc "jump to todays journal" "j j" 'jump-to-todays-journal
      :desc "jump to todays logbook" "j l" 'jump-to-todays-logbook
      :desc "jump to new journal" "j J" 'jump-to-new-journal
      :desc "jump to new logbook" "j L" 'jump-to-new-logbook)

(map! :v "C-c C-c" 'wsl-copy)
(map! :v "C-c C-v" 'wsl-paste)

(map! :leader
      (:prefix-map ("a" . "applications")
       (:prefix ("r" . "repoutil")
        :desc "Status of all branches" "b" #'repoutil-branchstat
        :desc "Fetch all branches" "f" #'repoutil-fetch
        :desc "List all managed repos" "l" #'repoutil-list
        :desc "List all unclean repos" "u" #'repoutil-unclean)
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
(map! "<f4>" '(lambda () (interactive) (org-agenda nil "cr")))
(map! :map org-mode-map :n "<SPC> m r a" 'org-change-state-and-archive)
(map! :map org-mode-map :n "<SPC> m r A" 'org-archive-to-archive-sibling)
(map! :map org-mode-map :n "<SPC> m d i" 'org-time-stamp-inactive)
(map! :map org-mode-map :n "<SPC> o s" 'org-open-link-same-window)
(map! :map org-mode-map :n "<SPC> o o" 'org-open-at-point)
(map! :map org-mode-map :n "<SPC> N" 'org-toggle-narrow-to-subtree)
(map! :map org-mode-map
      :n "C-x C-n" 'org-file-from-subtree
      :v "C-x C-n" 'org-file-from-selection)

(map! "<f5>" #'(lambda () (interactive) (find-next-file nil)))
(map! "<f6>" #'(lambda () (interactive) (find-next-file t)))

(map! :map dired-mode-map :n "/" 'dired-narrow)

(map! :nv "j" 'evil-next-visual-line
      :nv "k" 'evil-previous-visual-line)
(map! :leader :prefix "w" :desc "evil-window-split (follow)"
      "s" (lambda () (interactive) (+evil-window-split-a) (evil-window-down 1)))
(map! :leader :prefix "w" :desc "evil-window-vsplit (follow)"
      "v" (lambda () (interactive) (+evil-window-vsplit-a) (evil-window-right 1)))

(map! :map org-mode-map :n "<SPC> m l u" 'org-copy-link-url)



(after! org-roam
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-capture" "c" #'org-roam-capture))
