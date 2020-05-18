;;; init --- My emacs configuration

;;; Commentary:

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.



(require 'org)
(progn
  (setq gc-cons-threshold 100000000)
  (let ((cd/config-org "~/code/dotfiles/.emacs.d/chris-davison.org")
        (cd/config-el "~/code/dotfiles/.emacs.d/chris-davison.el"))
    (org-babel-tangle-file cd/config-org cd/config-el)
    (load cd/config-el t))
  (setq gc-cons-threshold 800000))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#e8e8e8")
 '(company-quickhelp-color-foreground "#444444")
 '(compilation-message-face 'default)
 '(electric-indent-mode nil)
 '(highlight-changes-colors '("#ff8eff" "#ab7eff"))
 '(highlight-tail-colors
   '(("#323342" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#323342" . 100)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#8f4e8b" "#8f684e" "#c3a043" "#397460" "#54ab8e" "#20a6ab" "#3573b1" "#DC8CC3"))
 '(objed-cursor-color "#ff6c6b")
 '(org-agenda-files
   '("/home/davison/Dropbox/notes/address--tic--technology-innovation-centre.org" "/home/davison/Dropbox/notes/akrasia.org" "/home/davison/Dropbox/notes/algorithms.org" "/home/davison/Dropbox/notes/apathy.org" "/home/davison/Dropbox/notes/archive.org" "/home/davison/Dropbox/notes/art.org" "/home/davison/Dropbox/notes/avoiding-triviality.org" "/home/davison/Dropbox/notes/awk.org" "/home/davison/Dropbox/notes/babble-and-prune.org" "/home/davison/Dropbox/notes/baduk.org" "/home/davison/Dropbox/notes/bash.org" "/home/davison/Dropbox/notes/blackscale-brakes--session-1.org" "/home/davison/Dropbox/notes/blackscale-brakes-rpg.org" "/home/davison/Dropbox/notes/bloodbowl-2.org" "/home/davison/Dropbox/notes/books.org" "/home/davison/Dropbox/notes/calendar.org" "/home/davison/Dropbox/notes/chess.org" "/home/davison/Dropbox/notes/chinese-language.org" "/home/davison/Dropbox/notes/coffee.org" "/home/davison/Dropbox/notes/cognitive-biases.org" "/home/davison/Dropbox/notes/commuting-by-bike.org" "/home/davison/Dropbox/notes/cow-fertility.org" "/home/davison/Dropbox/notes/curriculum-vitae.org" "/home/davison/Dropbox/notes/cybele.org" "/home/davison/Dropbox/notes/data-hubs-lakes-and-reservoirs.org" "/home/davison/Dropbox/notes/data-visualisation-tips.org" "/home/davison/Dropbox/notes/digital-filters.org" "/home/davison/Dropbox/notes/discredited-popular-science.org" "/home/davison/Dropbox/notes/docker.org" "/home/davison/Dropbox/notes/dungeon-world.org" "/home/davison/Dropbox/notes/dungeonmaster-tips.org" "/home/davison/Dropbox/notes/economics.org" "/home/davison/Dropbox/notes/eurorack.org" "/home/davison/Dropbox/notes/everyone-is-john-rpg.org" "/home/davison/Dropbox/notes/field-boundaries.org" "/home/davison/Dropbox/notes/finance.org" "/home/davison/Dropbox/notes/game-theory.org" "/home/davison/Dropbox/notes/git.org" "/home/davison/Dropbox/notes/glasgow.org" "/home/davison/Dropbox/notes/golang.org" "/home/davison/Dropbox/notes/great-music.org" "/home/davison/Dropbox/notes/groceries.org" "/home/davison/Dropbox/notes/guitar.org" "/home/davison/Dropbox/notes/haskell.org" "/home/davison/Dropbox/notes/health-and-fitness.org" "/home/davison/Dropbox/notes/home-ownership.org" "/home/davison/Dropbox/notes/ideas-to-revisit.org" "/home/davison/Dropbox/notes/imagemagick.org" "/home/davison/Dropbox/notes/inbox.org" "/home/davison/Dropbox/notes/internet-of-things.org" "/home/davison/Dropbox/notes/italics-in-terminal.org" "/home/davison/Dropbox/notes/julia.org" "/home/davison/Dropbox/notes/korean-language.org" "/home/davison/Dropbox/notes/language-learning.org" "/home/davison/Dropbox/notes/learn-music-by-ear.org" "/home/davison/Dropbox/notes/learning.org" "/home/davison/Dropbox/notes/linux.org" "/home/davison/Dropbox/notes/literature-reviews.org" "/home/davison/Dropbox/notes/literature.org" "/home/davison/Dropbox/notes/logbook.org" "/home/davison/Dropbox/notes/mac-osx.org" "/home/davison/Dropbox/notes/machine-learning.org" "/home/davison/Dropbox/notes/makefile-for-scientific-research.org" "/home/davison/Dropbox/notes/manpages.org" "/home/davison/Dropbox/notes/mastitis.org" "/home/davison/Dropbox/notes/mathematics.org" "/home/davison/Dropbox/notes/meditation.org" "/home/davison/Dropbox/notes/megaman-11.org" "/home/davison/Dropbox/notes/microsoft-word.org" "/home/davison/Dropbox/notes/minimalist-travel.org" "/home/davison/Dropbox/notes/monster-hunter-4-ultimate.org" "/home/davison/Dropbox/notes/morality.org" "/home/davison/Dropbox/notes/music-practice.org" "/home/davison/Dropbox/notes/my-hard-drives.org" "/home/davison/Dropbox/notes/my-models-and-algorithms.org" "/home/davison/Dropbox/notes/my-repositories.org" "/home/davison/Dropbox/notes/my-software-licenses.org" "/home/davison/Dropbox/notes/new-computer-setup.org" "/home/davison/Dropbox/notes/nintendo-switch-games.org" "/home/davison/Dropbox/notes/notable-dates.org" "/home/davison/Dropbox/notes/op-z.org" "/home/davison/Dropbox/notes/open-drone-map.org" "/home/davison/Dropbox/notes/pc-games.org" "/home/davison/Dropbox/notes/people.org" "/home/davison/Dropbox/notes/perl.org" "/home/davison/Dropbox/notes/philosophy.org" "/home/davison/Dropbox/notes/photogrammetry.org" "/home/davison/Dropbox/notes/places-to-visit.org" "/home/davison/Dropbox/notes/politics.org" "/home/davison/Dropbox/notes/portuguese-language-notes.org" "/home/davison/Dropbox/notes/postdoc-tips.org" "/home/davison/Dropbox/notes/programming.org" "/home/davison/Dropbox/notes/psychology.org" "/home/davison/Dropbox/notes/python.org" "/home/davison/Dropbox/notes/quotes.org" "/home/davison/Dropbox/notes/radio-spectrum-bands.org" "/home/davison/Dropbox/notes/rationality.org" "/home/davison/Dropbox/notes/reading-list-work.org" "/home/davison/Dropbox/notes/reading-music.org" "/home/davison/Dropbox/notes/recipes.org" "/home/davison/Dropbox/notes/run-jupyter-on-server.org" "/home/davison/Dropbox/notes/rust.org" "/home/davison/Dropbox/notes/saturday-white-bread.org" "/home/davison/Dropbox/notes/seoul-holiday-2013.org" "/home/davison/Dropbox/notes/social-science.org" "/home/davison/Dropbox/notes/social-skills.org" "/home/davison/Dropbox/notes/songs-to-learn.org" "/home/davison/Dropbox/notes/spanish-language-roadmap.org" "/home/davison/Dropbox/notes/structuring-a-thesis.org" "/home/davison/Dropbox/notes/stuff-to-learn.org" "/home/davison/Dropbox/notes/synthetic-aperture-radar.org" "/home/davison/Dropbox/notes/tabletop-games.org" "/home/davison/Dropbox/notes/tattoo-ideas.org" "/home/davison/Dropbox/notes/technical-writing.org" "/home/davison/Dropbox/notes/technology-readiness-level.org" "/home/davison/Dropbox/notes/the-doomsday-algorithm---determine-any-day-of-the-week.org" "/home/davison/Dropbox/notes/the-importance-of-writing.org" "/home/davison/Dropbox/notes/the-sentinel-satellite-project.org" "/home/davison/Dropbox/notes/thought-highlights.org" "/home/davison/Dropbox/notes/thought-prompts.org" "/home/davison/Dropbox/notes/tips-for-academia.org" "/home/davison/Dropbox/notes/todo.org" "/home/davison/Dropbox/notes/travel-history.org" "/home/davison/Dropbox/notes/travel-tips.org" "/home/davison/Dropbox/notes/tv-shows-and-films.org" "/home/davison/Dropbox/notes/typical-web-architecture.org" "/home/davison/Dropbox/notes/vim.org" "/home/davison/Dropbox/notes/visualisation.org" "/home/davison/Dropbox/notes/voluntary-milking-systems.org" "/home/davison/Dropbox/notes/want.org" "/home/davison/Dropbox/notes/warframe.org" "/home/davison/Dropbox/notes/web-and-javascript.org" "/home/davison/Dropbox/notes/what-is-an-engd.org" "/home/davison/Dropbox/notes/work.org" "/home/davison/Dropbox/notes/writing-abstracts.org" "/home/davison/Dropbox/notes/writing.org"))
 '(package-selected-packages
   '(ace-window which-key helm-org-rifle breadcrumb corral smartparens flycheck-clojure parinfer paredit-mode engine-mode cyberpunk-2019-theme cyberpunk-theme company-racer company-bibtex ob-sh org-ref org-recur projectile hydra-posframe evil-org evil-iedit-state evil-commentary evil deadgrep ivy-hydra deft lsp-mode org-sidebar electric-indent nov dired-single gist undo-tree company-try-hard hide-lines rainbow-delimiters rotate helpful visual-fill-column fold-dwim-org fold-dwim yafolding doom-modeline doom-themes darkokai-theme doneburn-theme hydra org-download htmlize exec-path-from-shell switch-window avy dumb-jump imenu-anywhere counsel ivy js2-mode sass-mode company-anaconda anaconda-mode pyvenv racer cargo flycheck-rust flymake-rust rust-mode go-mode flycheck company-quickhelp company anzu aggressive-indent yasnippet-snippets yasnippet iedit expand-region forge magit diminish f s fullframe use-package))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(weechat-color-list
   (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:italic t :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:italic t :strike-through t)))))
