;;; ../code/dotfiles/.doom.d/+orgcapture.el -*- lexical-binding: t; -*-

(setq org-capture-templates
      (doct `((,(emoji-heading 'all-the-icons-octicon "checklist" "Todo")
               :keys "t"
               :file "~/Dropbox/org/projects/todo.org"
               :template "* TODO %?")

              ;; 'journal' / 'note' disabled, now I'm trying org-journal
              ;; as keybind =SPC n j= jumps to journal file and creates a new
              ;; header with a timestamp
              ;; (,(emoji-heading 'all-the-icons-faicon "sticky-note-o" "Note")
              ;;  :keys "n"
              ;;  :file "journal.org"
              ;;  :datetree t
              ;;  :children (("List item" :keys "n" :type item :template "- %?")
              ;;             ("List link" :keys "l" :type item
              ;;              :template "- [[%c][%^{Description}]] %?")
              ;;             ("Entry" :keys "N" :type entry :template "* %?")))

              (,(emoji-heading 'all-the-icons-octicon "repo" "Logbook")
               :keys "l"
               :file "projects/work.org"
               :datetree t
               :children (("Item" :keys "l" :type item :template "- %?")
                          ("Entry" :keys "L" :type entry :template "* %?")
                          ("Dated entry" :keys "d" :type entry :template "* %?"
                           :time-prompt t)
                          ("Cybele" :keys "c" :type item :template "- %?"
                           :file "projects/cybele.org")
                          ("GlasData" :keys "g" :type item :template "- %?"
                           :file "projects/glasdata.org")))

              (,(emoji-heading 'all-the-icons-faicon "gamepad" "Gaming")
               :keys "g"
               :template "* TODO %?"
               :headline "UNFILED"
               :children (("PC" :keys "p" :file "pc-gaming.org")
                          ("Nintendo Switch" :keys "n" :file "nintendo-switch.org")
                          ("Tabletop" :keys "t" :file "tabletop-games.org")))

              (,(emoji-heading 'all-the-icons-faicon "television" "Watch")
               :keys "w"
               :file "projects/media.org"
               :headline "UNFILED"
               :template "* TODO %?")

              (,(emoji-heading 'all-the-icons-octicon "book" "Book")
               :keys "b"
               :file "projects/reading.org"
               :headline "REFILE"
               :template "* TODO %^{Book}\n%^{AUTHOR}p")

              (,(emoji-heading 'all-the-icons-octicon "comment" "Quote")
               :keys "Q"
               :file "journal.org"
               :type item
               :datetree t
               :template "- /\"%?\"/")

              (,(emoji-heading 'all-the-icons-faicon "graduation-cap" "Literature")
               :keys "L"
               :file "literature.org"
               :headline "REFILE"
               :template "* TODO %(read-capitalized-title)\n\nAuthors: %(read-authors)\n\n#+BEGIN_SRC bibtex\n#+END_SRC"
               :immediate-finish t))))
(setq cd/my-capture-templates org-capture-templates)

;; org journal seems to be introducing its own capture templates so try this
;; hack to see if I can force my own capture templates back into existence
(after! org-journal
  (setq org-capture-templates cd/my-capture-templates))
