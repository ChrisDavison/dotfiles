;;; ../code/dotfiles/.doom.d/+keybinds.el -*- lexical-binding: t; -*-

(map! "C-<" 'avy-goto-word-1) ;; C-S-,
;; (map! :leader "j" 'jump-to-register)
(map! :leader
      :desc "jump to here anchor" "j h" 'jump-to-here-anchor
      :desc "jump to todays journal" "j j" 'jump-to-todays-journal
      :desc "jump to logbook" "j l" 'logbook
      :desc "jump to new journal" "j J" 'jump-to-new-journal
      )

(map! :v
      "C-c C-c" 'wsl-copy
      "C-c C-v" 'wsl-paste)

(map! :leader
      (:prefix-map ("a" . "applications")
       (:prefix ("r" . "repoutil")
        :desc "Status of all branches" "b" #'repoutil-branchstat
        :desc "Fetch all branches" "f" #'repoutil-fetch
        :desc "List all managed repos" "l" #'repoutil-list
        :desc "List all unclean repos" "u" #'repoutil-unclean)))

;; Test editing
(map! :n "C-;" #'iedit-mode
      :n "C-:" #'iedit-mode-toggle-on-function)

(map! "M-%" #'anzu-query-replace
      "C-M-%" #'anzu-query-replace-regexp)

;; Emacs capture and org-mode
(map! "<f1>" 'org-capture
      "<f2>" 'org-agenda
      "<f3>" '(lambda () (interactive) (org-agenda nil "c1"))
      "<f4>" '(lambda () (interactive) (org-agenda nil "cr")))

(map! :map org-mode-map :leader :n
      "m r a" 'org-change-state-and-archive
      "m r A" 'org-archive-to-archive-sibling
      "m d i" 'org-time-stamp-inactive
      "o s" 'org-open-link-same-window
      "o o" 'org-open-at-point
      "Q" 'org-unfill-paragraph
      "N" 'org-toggle-narrow-to-subtree)
(map! :map org-mode-map :n
      "C-x C-n" 'org-file-from-subtree
      :v "C-x C-n" 'org-file-from-selection)

(map! :map rust-mode-map :leader :n "c d" 'racer-find-definition)

(map! "<f5>" #'find-next-file
      "<f6>" #'find-previous-file)

(map! :map dired-mode-map :n "/" 'dired-narrow)

(map! :nv "j" 'evil-next-visual-line
      :nv "k" 'evil-previous-visual-line)

(map! :leader
      :prefix "w"
      :desc "evil-window-split (follow)" "s" (lambda () (interactive)
        (evil-window-split)
        (evil-window-down 1))
      :desc "evil-window-vsplit (follow)" "v" (lambda () (interactive)
        (evil-window-vsplit)
        (evil-window-right 1)))

(map! :map org-mode-map :n "<SPC> m l u" 'org-copy-link-url)

(map! :after projectile
      :leader
      :desc "Find Dropbox note" "<SPC>" #'(lambda () (interactive) (counsel-file-jump nil org-directory)))

