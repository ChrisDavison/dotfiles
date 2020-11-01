;;; ../code/dotfiles/.doom.d/+orgcapture.el -*- lexical-binding: t; -*-

(setq cd/capture-literature
      `("L" ,(emoji-heading 'all-the-icons-faicon "graduation-cap" "Literature")
        entry (file+headline "literature.org" "REFILE")
        "* TODO %(read-capitalized-title)\n\nAuthors: %(read-authors)\n\n#+BEGIN_SRC bibtex\n#+END_SRC"))

(setq cd/org-capture-templates
      (doct
       `(("Todo"
          :keys "t"
          :file "projects/todo.org"
          :template "* TODO %?")

         ("Work"
          :keys "w"
          :template "* TODO %?"
          :headline "Tasks"
          :children (("General" :keys "w" :file "projects/work.org")
                     ("GlasData" :keys "g" :file "projects/glasdata.org")
                     ("Cybele" :keys "c" :file "projects/cybele.org")))

         ("JOURNAL"
          :keys "j"
          :file "journal.org"
          :children (("Logbook Item" :keys "j" :type item :template "- %?")
                     ("Logbook Entry" :keys "J" :template "* %?" :datetree t)))

        ("LOGBOOK"
          :keys "l"
          :file "projects/work.org"
          :children (("Logbook Item" :keys "l" :type item :template "- %?")
                     ("Logbook Entry" :keys "L" :template "* %?" :datetree t)))

        ("GAMING"
        :keys "g"
        :headline "UNFILED"
        :template "* TODO %?"
        :children (("PC" :keys "p" :file "pc-gaming.org")
                   ("Nintendo Switch" :keys "n" :file "nintendo-switch.org")
                   ("Tabletop" :keys "t" :file "tabletop-games.org")))

        ("MEDIA"
         :keys "m"
         :file "projects/media.org"
         :children (("Watch" :keys "w" :headline "UNFILED" :template "* TODO %?")
                    ("Music" :keys "m" :headline "Music" :template "* TODO %?")))

        ("Guitar song to learn"
         :keys "G"
         :file "projects/guitar.org" :headline "Songs to Learn"
         :template "* TODO %?")

        ("Books / reading"
         :keys "b"
         :file "projects/reading.org" :headline "REFILE"
         :template "* TODO %?")

        ("Anki"
         :keys "a"
         :file "projects/todo.org" :headline "anki"
         :template "* TODO %?")
         )))

(setq org-capture-templates cd/org-capture-templates)

;; Make capture windows take of 90% of the frame
(set-popup-rule! "^CAPTURE" :side 'bottom :size 0.90 :select t :ttl nil)
