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
  (setq org-directory "~/Dropbox/notes")
  (setq org-default-notes-file "~/Dropbox/notes/inbox.org")
  (setq org-src-window-setup 'current-window)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files
        (delq nil (mapc (lambda (x) (and (file-exists-p x) x))
                        '("~/Dropbox/notes/"))))
  (setq org-todo-keywords 
        '((sequence "-TODO-" "-WIP-" "|" "-DONE-" "-CANCELLED-")))
  (setq org-startup-indented t)


  ;; Settings for refiling
  (setq org-reverse-note-order t)
  (setq org-refile-use-outline-path nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache nil)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (setq org-blank-before-new-entry nil)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq fill-column 80))

(use-package ox-reveal :ensure t)
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
      '(("q" "quotes" entry
         (file "~/Dropbox/notes/quotes.org")
         "* %^{WHAT} ** %^{WHO? WHERE?}\n%^{QUOTE}"
         :immediate-finish)

        ("t" "todo" item
         (file "~/Dropbox/notes/todo.org")
         "- %^{PRIORITY} %^{WHEN} %^{TASK} %^{CONTEXT} %^{TAGS}"
         :immediate-finish)

        ("e" "engd" entry
         (file+headline "~/Dropbox/notes/inbox.org" "EngD")
         "** %^{WHAT}\n%?"
         :immediate-finish)

        ("l" "logbook" plain
         (file+datetree "~/Dropbox/notes/engd/logbook2016.org")
         "%^{LOGBOOK ENTRY}\n\n"
         :immediate-finish)

        ("n" "note" item
         (file+headline "~/Dropbox/notes/inbox.org" "REFILE")
         "%^{NOTE}\n"
         :immediate-finish)

        ("p" "plain note" plain
         (file+headline "~/Dropbox/notes/inbox.org" "REFILE")
         "%^{NOTE}\n"
         :immediate-finish)


        ("u" "url" item
         (file+headline "~/Dropbox/notes/inbox.org" "REFILE")
         "[[%^{URL}][%^{DESCRIPTION}]]\n"
         :immediate-finish)
        ))


;; indent org babel src
In an Org-Babel block, run my/org-cleanup to fix indentation

(defun my/org-cleanup ()
  (interactive)
  (org-edit-special)
  (indent-buffer)
  (org-edit-src-exit))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-x c") 'my/org-cleanup)


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

;; html exporting

;; (setq org-html-head
;;       "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.pirilampo.org/styles/readtheorg/css/htmlize.css\"/>
;;             <link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.pirilampo.org/styles/readtheorg/css/readtheorg.css\"/>
;;             <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>
;;             <script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>
;;             <script type=\"text/javascript\" src=\"http://www.pirilampo.org/styles/lib/js/jquery.stickytableheaders.js\"></script>
;;             <script type=\"text/javascript\" src=\"http://www.pirilampo.org/styles/readtheorg/js/readtheorg.js\"></script> ")
;; ,#+HTML_HEAD: <link rel="stylesheet" type="text/css" href="http://www.pirilampo.org/styles/bigblow/css/htmlize.css"/>
;; ,#+HTML_HEAD: <link rel="stylesheet" type="text/css" href="http://www.pirilampo.org/styles/bigblow/css/bigblow.css"/>
;; ,#+HTML_HEAD: <link rel="stylesheet" type="text/css" href="http://www.pirilampo.org/styles/bigblow/css/hideshow.css"/>

;; ,#+HTML_HEAD: <script type="text/javascript" src="http://www.pirilampo.org/styles/bigblow/js/jquery-1.11.0.min.js"></script>
;; ,#+HTML_HEAD: <script type="text/javascript" src="http://www.pirilampo.org/styles/bigblow/js/jquery-ui-1.10.2.min.js"></script>

;; ,#+HTML_HEAD: <script type="text/javascript" src="http://www.pirilampo.org/styles/bigblow/js/jquery.localscroll-min.js"></script>
;; ,#+HTML_HEAD: <script type="text/javascript" src="http://www.pirilampo.org/styles/bigblow/js/jquery.scrollTo-1.4.3.1-min.js"></script>
;; ,#+HTML_HEAD: <script type="text/javascript" src="http://www.pirilampo.org/styles/bigblow/js/jquery.zclip.min.js"></script>
;; ,#+HTML_HEAD: <script type="text/javascript" src="http://www.pirilampo.org/styles/bigblow/js/bigblow.js"></script>
;; ,#+HTML_HEAD: <script type="text/javascript" src="http://www.pirilampo.org/styles/bigblow/js/hideshow.js"></script>
;; ,#+HTML_HEAD: <script type="text/javascript" src="http://www.pirilampo.org/styles/lib/js/jquery.stickytableheaders.min.js"></script>

;; agenda
(setq org-agenda-files '("~/Dropbox/notes/inbox.org"
                         "~/Dropbox/notes/engd/logbook2016.org"
                         "~/Dropbox/notes/todo.org"))


(provide 'my-org)
;;; my-org.el ends here
