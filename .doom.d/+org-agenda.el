;;; ../code/dotfiles/.doom.d/+orgagenda.el -*- lexical-binding: t; -*-

(setq org-agenda-custom-commands
      '(("c" . "+my stuff")
        ("c1" "One day" ((agenda ""
                                 ((org-agenda-span 'day)
                                  (org-agenda-start-day "-0d")))))
        ("cm" "Media"
         ((todo ""
                ((org-agenda-files '("~/Dropbox/org/projects/media.org"))))))
        ("cw" "Work" ((todo ""
                            ((org-agenda-files '("~/Dropbox/org/projects/work.org"
                                                 "~/Dropbox/org/projects/cybele.org"
                                                 "~/Dropbox/org/projects/glasdata-collaboration.org"
                                                 ))
                             (org-agenda-overriding-header "Work")))))

        ("cW" "Weekly Review (last 7 days' DONE)"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-day "-7d")
                      (org-agenda-entry-types '(:timestamp))
                      (org-agenda-show-log t)))))
        ("cf" "Weekly Review (last 7 days' DONE, from Friday)"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-day "-6d")
                      (org-agenda-entry-types '(:timestamp))
                      (org-agenda-show-log t)))))))

(setq org-agenda-sorting-strategy
      '(
        (agenda habit-down time-up todo-state-up priority-down)
        (todo todo-state-down priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)
        ))
