;;; my-org.el --- Org-mode

;;; Commentary:
;;;
;;; Powerful note taking with org-mode

;;; Code:

;; taking notes
;; Set up org for taking notes, using Dropbox/docs as my main
;; folder. Also, set up some nice config for org todo and agenda
;; stuff. =org-refile= lets you organize notes by typing in the
;; headline to file them under.

(global-set-key (kbd "C-c q") 'auto-fill-mode)

(use-package org
  :ensure t
  :bind (("<f1>" . org-capture)
         ("<f2>" . org-agenda)
         ("<f3>" . org-agenda-list)
         ("<f4>" . org-timeline))
  :config
  (setq org-directory "~/Dropbox/notes"
        org-default-notes-file "~/Dropbox/inbox.org"
        org-src-window-setup 'current-window
        org-src-fontify-natively t
        org-todo-keywords '((sequence "-TODO-" "-WIP-" "|" "-DONE-" "-CANCELLED-"))
        org-startup-indented t
        org-agenda-files (list "~/Dropbox/"))
  ;; Settings for refiling
  (setq org-reverse-note-order t
        org-refile-use-outline-path nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-cache nil
        org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
        org-blank-before-new-entry nil)
  ;; (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq fill-column 80))

;; (use-package ox-reveal :ensure t)
(use-package htmlize :ensure t)
(use-package org-bullets :ensure t
  :disabled f
  :config (org-bullets-mode 1))

;; This makes it easier to add links from outside.
(defun sacha/yank-more ()
  "Yank into an org link."
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'sacha/yank-more)


;; Paste a link into an org file document, using the currently
;; selected text as the description

;; Something like Sacha Chua's yank-more, but using the current region
;; as 'more'

;; Below is /kind of/ along the right lines, but need to make it work
;; with either the kill ring or the clipboard, as well as removing the
;; current region.

;; (setq save-interprogram-paste-before-kill t)

;; (defun cd/yank-with-selection-as-description (title)
;;   (interactive "MLink Title: \n")
;;   (insert "[[")
;;   (clipboard-yank)
;;   (insert "][")
;;   (insert title)
;;   insert "]]"
;;   (message "Yanked with Selection"))

;; (global-set-key (kbd "<f7>") 'cd/yank-with-selection-as-description)

;; capture - templates
;; =org-capture= lets you create templates for jotting down info of
;; various kinds.
(setq org-capture-templates
      '(("q" "quotes" entry (file "~/Dropbox/reference/quotes.org")
         "* %^{WHAT} ** %^{WHO? WHERE?}\n%^{QUOTE}" :immediate-finish)
        ("u" "url" item (file+headline "~/Dropbox/inbox.org" "Links")
         "[[%^{URL}][%^{DESCRIPTION}]]\n")
        ;; Header-bullet of -TODO- <TASK>, under the TASKS L1 header
        ("t" "todo" entry (file+headline "~/Dropbox/inbox.org" "TASKS")
         "* -TODO- %^{TASK}")
        ;; Datetree of YYYY / YYYY-MM MONTHNAME / YYYY-MM-DD DAYNAME
        ("j" "Journal" entry (file+datetree "~/Dropbox/journal.org")
         "**** %^{BLAH} \n")
        ("l" "Time-logged Journal" entry (file+datetree "~/Dropbox/journal.org")
         "**** %U %^{BLAH} \n")
        )
      )



;; indent org babel src
;; In an Org-Babel block, run my/org-cleanup to fix indentation

(defun cd/org-cleanup ()
  (interactive)
  (org-edit-special)
  (indent-buffer)
  (org-edit-src-exit))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-x c") 'cd/org-cleanup)


;; babel

(setq org-babel-load-languages
      '((emacs-lisp . t)
        (R . t)
        (Python . t)))

(setq org-confirm-babel-evaluate nil)

(define-skeleton org-skeleton
  "Header info for a emacs-org file."
  "-----\n"
  "#+TITLE: " (skeleton-read "Title: ") "\n"
  "#+AUTHOR: Chris Davison\n"
  "#+EMAIL: c.jr.davison@gmail.com\n"
  "#+OPTIONS: toc:2 num:nil html-postamble:nil\n"
  "#+PROPERTY: header-args :tangle " (skeleton-read "Tangle filename: ") "\n")
;;(global-set-key [C-S-f4] 'org-skeleton)

;; latex exporting

(require 'ox-latex)

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("simple"
               "\\documentclass[a4paper, 12pt, article]{article}
                  \\usepackage[top=0.5in, bottom=0.5in, left=0.5in, right=0.5in]{geometry}
                  \\usepackage{mathpazo}
                  \\usepackage{cite}
                  \\usepackage{color}
                  \\usepackage[unicode=true]{hyperref}
                  \\hypersetup{breaklinks=true, bookmarks=true, pdfauthor={}, pdftitle={}, colorlinks=true, citecolor=blue, urlcolor=blue, linkcolor=magenta, pdfborder={0 0 0}}
                  [DEFAULT-PACKAGES]
                  [PACKAGES]
                  "
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("cd"
               "\\documentclass[12pt, a4paper]{article}
                  \\usepackage{setspace} % Line spacing
                  \\singlespacing
                  % \\onehalfspacing
                  % \\doublespacing
                  % \\setstretch{1.25}

                  % Set Margins
                  \\usepackage[left=2cm, right=2cm, top=2cm, bottom=4cm]{geometry}

                  % Set paragraph indents to 1 cm
                  \\setlength{\\parindent}{0.5cm}

                  % Set no space between paragraphs
                  \\setlength{\\parskip}{0ex}

                  % No extra space between words
                  \\frenchspacing

                  % Set up margin notes
                  \\usepackage{mparhack}
                  \\newcommand{\\marginnote}[1]{\\marginpar{\\vspace{-8ex}\\singlespacing\\raggedright\\scriptsize{#1}}}

                  % Prevent over-eager hyphenation
                  % \\hyphenpenalty=5000
                  \\tolerance=1000

                  % Captions left justified
                  \\usepackage[format=plain,labelsep=newline,singlelinecheck=false,font={small},labelfont={small,bf}]{caption}[2008/04/01]

                  % Pretty tables
                  \\usepackage{mdwtab}
                  % Long tables
                  \\usepackage{longtable, mdwtab}
                  \\usepackage{longtable}
                  % Don't indent longtables by a parindent!
                  \\setlength\\LTleft{0pt}
                  \\setlength\\LTright{0pt}

                  % Multirow cells in tables
                  \\usepackage{multirow}

                  % Maths
                  \\usepackage[fleqn]{amsmath} % All equations left justified
                  \\usepackage{amsfonts}
                  \\usepackage{amsthm}
                  \\usepackage{stmaryrd}
                  \\allowdisplaybreaks
                  \\usepackage{latexsym}
                  \\usepackage{bm}
                  \\usepackage{dsfont}
                  \\usepackage{mathrsfs}
                  \\usepackage{xfrac} %  \\sfrac
                  %% QED symbol
                  \\renewcommand{\\qedsymbol}{$\\blacksquare$}


                  % Subfigures
                  % \\usepackage{subfig}
                  \\usepackage{subcaption}

                  % URLs
                  \\usepackage{url}

                  % Compact lists
                  \\newenvironment{itemize*}%
                  {\\begin{itemize}%
                      \\setlength{\\itemsep}{0.5pt}%
                      \\setlength{\\parskip}{0.5pt}}%
                  {\\end{itemize}}

                  \\newenvironment{enumerate*}%
                  {\\begin{enumerate}%
                      \\setlength{\\itemsep}{0.5pt}%
                      \\setlength{\\parskip}{0.5pt}}%
                  {\\end{enumerate}}

                  % Set figure size
                  \\newcommand{\\figsize}{4.5in}

                  % Numbered subsubsections
                  \\setcounter{secnumdepth}{3}

                  % Annotate correction
                  \\usepackage{color}
                  \\newcommand{\\annotate}[2]{\\marginpar{{\\color{red}\\textbf{#1}}}{\\color{red}\\emph{#2}}}


                  \\newcommand{\\divider}{\\begin{center}\\line(1,0){250}\\end{center}}
                  [DEFAULT-PACKAGES]
                  [PACKAGES]
                  "
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;; This function makes checkbox counting work with HEADER checkboxes, as well as sublists.
(defun wicked/org-update-checkbox-count (&optional all)
  "Update the checkbox statistics in the current section.
This will find all statistic cookies like [57%] and [6/12] and update
them with the current numbers.  With optional prefix argument ALL,
do this for the whole buffer."
  (interactive "P")
  (save-excursion
    (let* ((buffer-invisibility-spec (org-inhibit-invisibility)) 
           (beg (condition-case nil
                    (progn (outline-back-to-heading) (point))
                  (error (point-min))))
           (end (move-marker
                 (make-marker)
                 (progn (or (outline-get-next-sibling) ;; (1)
                            (goto-char (point-max)))
                        (point))))   
           (re "\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)")
           (re-box
            "^[ \t]*\\(*+\\|[-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
           b1 e1 f1 c-on c-off lim (cstat 0))
      (when all
        (goto-char (point-min))
        (or (outline-get-next-sibling) (goto-char (point-max))) ;; (2)
        (setq beg (point) end (point-max)))
      (goto-char beg)
      (while (re-search-forward re end t)
        (setq cstat (1+ cstat)
              b1 (match-beginning 0)
              e1 (match-end 0)
              f1 (match-beginning 1)
              lim (cond
                   ((org-on-heading-p)
                    (or (outline-get-next-sibling) ;; (3)
                        (goto-char (point-max)))
                    (point))
                   ((org-at-item-p) (org-end-of-item) (point))
                   (t nil))
              c-on 0 c-off 0)
        (goto-char e1)
        (when lim
          (while (re-search-forward re-box lim t)
            (if (member (match-string 2) '("[ ]" "[-]"))
                (setq c-off (1+ c-off))
              (setq c-on (1+ c-on))))
          (goto-char b1)
          (insert (if f1
                      (format "[%d%%]" (/ (* 100 c-on)
                                          (max 1 (+ c-on c-off))))
                    (format "[%d/%d]" c-on (+ c-on c-off))))
          (and (looking-at "\\[.*?\\]")
               (replace-match ""))))
      (when (interactive-p)
        (message "Checkbox statistics updated %s (%d places)"
                 (if all "in entire file" "in current outline entry")
                 cstat)))))
(defadvice org-update-checkbox-count (around wicked activate)
  "Fix the built-in checkbox count to understand headlines."
  (setq ad-return-value
        (wicked/org-update-checkbox-count (ad-get-arg 1))))

(provide 'my-org)
;;; my-org.el ends here
