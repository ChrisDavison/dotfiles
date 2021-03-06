(require 'dash)  ;; Stuff like map, each, filter
(require 'f)  ;; Filepath functions
(require 's)  ;; String functions
(require 'rx) ;; Literate regular expressions

;; -----------------------------------------------------------------------------
;;; General settings
;; -----------------------------------------------------------------------------
(defvar my-code-dir
  (expand-file-name "~/src/github.com/ChrisDavison")
  "Where my code is stored.")
(defvar my-work-code-dir
  (expand-file-name "~/src/github.com/cidcom")
  "Where my WORK code is stored.")
(setq-default org-directory (f-join my-code-dir "knowledge"))
(setq-default org-roam-directory org-directory)

(setq user-full-name "Chris Davison"
      user-mail-address "c.jr.davison@gmail.com"
      auto-save-default t
      auto-save-timeout 5
      avy-all-windows t
      recentf-auto-cleanup 60
      global-auto-revert-mode t
      projectile-project-search-path `(,my-code-dir ,my-work-code-dir)
      display-line-numbers-type t
      search-invisible t  ;; don't skip matches in query-replace when hidden (e.g. org-mode link urls)
      nov-text-width 80)


(setq-default org-roam-directory (f-join my-code-dir "knowledge"))

(add-to-list 'auth-sources "~/.authinfo")
(add-hook! dired-mode 'dired-hide-details-mode)

(after! projectile
  (add-to-list 'projectile-project-root-files ".projectile-root"))

;; -----------------------------------------------------------------------------
;;; GLOBAL MODES
;; -----------------------------------------------------------------------------
(global-visual-line-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-undo-tree-mode 1)
(global-anzu-mode 1) ;; Live preview of search and replace (C-M-@)

;; -----------------------------------------------------------------------------
;;; Hooks
;; -----------------------------------------------------------------------------
(setq fill-column 120)
(add-hook 'prog-mode-hook #'undo-tree-mode)

;; LSP
(add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
(setq lsp-lens-enable t)
(setq +format-with-lsp nil)

(setq vterm-shell "/usr/bin/fish")
(setq shell-file-name "/usr/bin/fish")

;; Nov.el - read epubs in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("docker[a-z\-]" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.scratch\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))

(setq ibuffer-formats
      `((mark vc-status-mini " "
              (name 50 50 :left :elide) " "
              (size 9 -1 :right)
              " "
              (mode 10 -1 :left) " "
              )
        (mark vc-status-mini " "
              (name 30 30 :left :elide) " "
              (size 9 -1 :right)
              " "
              (mode 10 -1 :left) " "
              vc-relative-file)))

;; -----------------------------------------------------------------------------
;;; Programming - Rust
;; -----------------------------------------------------------------------------
(add-hook! rust-mode
           '(company-mode
             flycheck-rust-setup
             cargo-minor-mode
             racer-mode
             ))
(add-hook! racer-mode '(company-mode eldoc-mode))
(add-to-list 'auto-mode-alist '("\\.rs" . rust-mode))

;; -----------------------------------------------------------------------------
;;; Programming - Golang
;; -----------------------------------------------------------------------------
;; (add-to-list 'exec-path (concat (file-name-as-directory (getenv "GOPATH")) "bin") t)
;; (add-to-list 'load-path (concat (file-name-as-directory (getenv "GOPATH")) "src/github.com/dougm/goflymake"))
;; (require 'go-flymake)
                                        ; Use goimports instead of go-fmt for formatting with intelligent package addition/removal
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook
          '(lambda ()
             (set (make-local-variable 'company-backends) '(company-go))
             (local-set-key (kbd "M-.") 'godef-jump)
             (go-eldoc-setup)
                                        ; call Gofmt before saving
             (add-hook 'before-save-hook 'gofmt-before-save)))

;; -----------------------------------------------------------------------------
;;; Programming - Python
;; -----------------------------------------------------------------------------
(setq python-environment-directory "~/.envs/py"
      python-shell-interpreter "python"
      ;; python-shell-interpreter-args "console --simple-prompt"
      python-shell-interpreter-args ""
      elpy-rpc-python-command "~/.envs/py/bin/python")

(add-hook! 'pyvenv-post-activate-hooks
           '((lambda ()
               (setq python-shell-interpreter (f-join pyvenv-virtual-env "bin/jupyter")))))
(add-hook! 'pyvenv-post-deactivate-hooks
           '((lambda ()
               (setq python-shell-interpreter "python3"))))

(map! :map python-mode-map "C-c r" 'elpy-send-contiguous-block)

(setq lsp-imenu-index-symbol-kinds
      '(Class Method Property Field Constructor Enum Interface Function Struct Namespace))

;; -----------------------------------------------------------------------------
;;; Programming - Haskell
;; -----------------------------------------------------------------------------
(setq haskell-process-type 'stack-ghci)

(defun insert-formatted-time (format)
  "Insert a timestamp matching a specific format"
  (insert (format-time-string format (current-time))))

(defun insert-timestamp-long ()
  "Insert a LONG timestamp"
  (interactive)
  (insert-formatted-time "%a %b %d %H:%M:%S %Z %Y"))

(defun insert-timestamp-date ()
  "Insert a plain date"
  (interactive)
  (insert-formatted-time "%Y-%m-%d"))

(defun insert-timestamp-time ()
  "Insert a plain timestamp"
  (interactive)
  (insert-formatted-time "%H:%M:%S"))

(defun repoutil (command)
  (cd/shell-command-to-special-buf
   (format "repoutil %s" command)
   "*repoutil*"))
(set-popup-rule! "^\\*repoutil\\*" :side 'bottom :size 0.30 :select t :ttl 1)

(defun cd/shell-command-to-special-buf (command bufname)
  (get-buffer-create bufname)
  (message (format "Running: %s" command))
  (shell-command command bufname)
  (switch-to-buffer-other-window bufname)
  (special-mode)
  (evil-insert 1))

(defun repoutil-branchstat () (interactive) (repoutil "branchstat"))

(defun repoutil-list () (interactive) (repoutil "list"))

(defun repoutil-fetch () (interactive) (repoutil "fetch") (quit-window))

(defun repoutil-unclean () (interactive) (repoutil "unclean"))

(defun tagsearch-list (&optional tags)
  "List tags under the current directory.

When optional TAGS is a string, show only files matching those tags"
  (interactive)
  (let ((cmd (concat "tagsearch " (or tags "")))
        (temp-buf-name "*tagsearch*"))
    (get-buffer-create temp-buf-name)
    (shell-command cmd temp-buf-name)
    (switch-to-buffer-other-window temp-buf-name)
    (special-mode)
    (evil-insert 1)))

(set-popup-rule! "^\\*tagsearch" :side 'bottom :size 0.30 :select t :ttl 1)

(defun files-matching-tagsearch (&optional tags directory)
  (interactive)
  (let* ((directory (if directory directory (read-directory-name "DIR: ")))
         (cmd (format "tagsearch %s | grep -v archive" (if tags tags (read-string "Tags: "))))
         (fullcmd (format "cd %s && %s" directory cmd))
         (output (s-split "\n" (s-trim (shell-command-to-string fullcmd)))))

    (get-buffer-create "*tagsearch*")
    (shell-command fullcmd "*tagsearch*")
    (switch-to-buffer-other-window "*tagsearch*")
    (special-mode)
    (evil-insert 1)))

(defun find-file-tagsearch (&optional tags directory)
  (interactive)
  (let* (
         (tags (or tags (read-string "Tags: ")))
         (default-directory (expand-file-name (or directory (read-directory-name "Dir: "))))
         (command (s-concat "tagsearch " tags))
         (files (s-split "\n" (s-trim (shell-command-to-string command))))
         (chosen (ivy-read (format "@%s: " tags) files))
         )
    (find-file (f-join default-directory chosen))
    ))

(defun cd/find-thought-file ()
  (interactive)
  (find-file-tagsearch "thought" org-directory))

(defun cd/find-index-file ()
  (interactive)
  (find-file-tagsearch "index" org-directory))

(defun cd/find-book-list-file ()
  (interactive)
  (find-file-tagsearch "booklist" org-directory))

(defun rg-journal (search)
  (interactive "Msearch string: ")
  (rg search "journal.org" (f-join my-code-dir "knowledge")))

(defun rg-logbook (search)
  (interactive "Msearch string: ")
  (rg search "logbook.org" (f-join my-code-dir "knowledge")))

(defun rg-org (search)
  (interactive "Msearch string: ")
  (rg search "org" org-directory))

(defun new-in-git (&optional n)
  (interactive)
  (let* ((bufname "*new-in-repo*")
         (n (if n n 7))
         (cmd (format "new_in_git %s" n)))
    (get-buffer-create bufname)
    (shell-command cmd bufname)
    (switch-to-buffer-other-window bufname)
    (special-mode)))
(set-popup-rule! "^\\*new-in-repo\\*" :side 'bottom :size 0.30 :select t :ttl 1)

(defun cd/nas/quick-add-download ()
  "Add contents of clipboard to nas' to-download file."
  (interactive)
  (let* ((path "/media/nas/to-download.txt")
         (clip (s-trim (current-kill 0)))
         (re-org-url "\\[\\[\\(.*\\)\\]\\[.*\\]\\]")
         (matches (s-match re-org-url clip))
         (url (if matches (cadr matches) clip))
         (url-tidy (if (s-matches? "youtube\\|youtu\.be" url)
                       (car (s-split "&" url))
                     url))
         (contents (s-split "\n" (read-file-to-string path))))
    (pushnew! contents url-tidy)
    (delete-dups contents)
    (write-region (s-join "\n" contents) nil path)
    (message (concat "Added to downloads: " url-tidy))))

(defun cd/nas/list-downloads ()
  "List contents of NAS 'to-download' list."
  (interactive)
  (let* ((path "/media/nas/to-download.txt")
         (temp-buf-name "*nas-downloads*"))
    (get-buffer-create temp-buf-name)
    (switch-to-buffer-other-window temp-buf-name)
    (insert "NAS DOWNLOADS\n=============\n")
    (insert-file-contents path)
    (special-mode)
    (evil-insert 1)))
(set-popup-rule! "^\\*nas-downloads*" :side 'bottom :size 0.30 :select t :ttl 1)

;;; Navigate narrows
(defun next-narrow (&optional backwards)
  (interactive)
  (progn
    (beginning-of-buffer)
    (widen)
    (if backwards (outline-previous-heading) (outline-next-heading))
    (org-narrow-to-subtree)))

(defun find-next-file (&optional backward)
  "Find the next file (by name) in the current directory.

With prefix arg, find the previous file."
  (interactive "P")
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                (sort (directory-files (file-name-directory file) t nil t) 'string<)))
           (direction (if backward -1 1))
           (pos (mod (+ (cl-position file files :test 'equal) direction)
                     (length files))))
      (find-file (nth pos files)))))

(defun find-previous-file ()
  "Find the next file (by name) in the current directory."
  (interactive)
  (find-next-file t))

(defun files-in-curdir-with-ext (ext)
  (let* ((curdir (expand-file-name default-directory))
         (files (directory-files curdir)))
    (seq-filter
     (lambda (filename)
       (s-equals? ext (file-name-extension filename)))
     (-map (lambda (file) (s-concat curdir file)) files))))

(defun cd/notes-from-last-n-days (&optional n)
  (interactive)
  (require 'ts)
  (let* ((n (if n n 7))
         ;; (files (find-lisp-find-files (f-join org-directory "journal") "\.org$"))
         (date-n-ago (ts-format "%F" (ts-adjust 'day (- 0 n) (ts-now))))
         (files-last-n (--filter (string-greaterp (car (s-split "--" (file-name-base it))) date-n-ago)
                                 files))
         (sorted-files (sort files-last-n 'string-greaterp))
         (bufname "*recent-notes*"))
    (get-buffer-create bufname)
    (switch-to-buffer-other-window bufname)
    (erase-buffer)
    (org-mode)
    (insert "* Git Additions\n\n")
    (let ((curdir default-directory))
      (cd org-directory)
      (insert (shell-command-to-string (format "new_in_git %d" n)))
      (cd curdir))
    (insert "\n")
    (--each sorted-files (insert-file it))
    (+org/close-all-folds)))

(defun cd/notes-from-last-week ()
  (interactive)
  (cd/notes-from-last-n-days 7))

(defun cd/notes-from-yesterday ()
  (interactive)
  (cd/notes-from-last-n-days 1))

;;; Tags (like tagsearch or roam)
(defun tagify (str)
  (interactive "M")
  (s-join " " (--map (format "@%s" it) (s-split " " str))))

(defun roam-tagify (str)
  (interactive "Mtags: ")
  (evil-open-below 1)
  (insert (format "#+ROAM_TAGS: %s\n\n" str))
  (insert (tagify str))
  (evil-force-normal-state)
  (save-buffer))

(defun roam-tagify-toplevel (str)
  (interactive "Mtags: ")
  (evil-goto-first-line)
  (evil-insert-line 1)
  (insert (s-concat "#+ROAM_TAGS: " (tagify str) "\n\n"))
  (evil-force-normal-state)
  (save-buffer))

(defun get-asset-dir ()
  (interactive)
  (let ((maybe-asset-dir (f-join (projectile-project-root) "assets")))
    (if (f-readable? maybe-asset-dir)
        maybe-asset-dir
      "./assets")))

(defun get-relative-asset-dir ()
  (interactive)
  (file-relative-name (get-asset-dir)
                      (buffer-file-name)))

;;; Lists and checkboxes
(defun make-into-list ()
  "Basically equivalent to org-ctrl-c-minus."
  (interactive)
  (replace-regexp "^" "- " nil (region-beginning) (region-end)))

(defun make-into-checkbox-list ()
  "Convert selection to list (only at root level) of checkboxes."
  (interactive)
  (let ((re (rx bol (zero-or-one "-") (one-or-more space))))
    (replace-regexp re "- [ ] " nil (region-beginning) (region-end))))

(defun cd/cycling-tss-summary ()
  (interactive)
  (let* ((fname (f-join org-directory "health-fitness-nutrition.org"))
         (contents (s-split "\n" (read-file-to-string fname)))
         (matching (--filter (or (s-matches? "[0-9]+ W[0-9]+" it)
                                 (s-matches? "Total.*stress" it))
                             contents))
         (pairs (map-pairs matching))
         (tidied (--map `(,(s-replace-regexp "^\*+ +" "" (car it))
                          ,(s-replace-regexp ".*:: +" "" (cdr it)))
                        pairs))
         (strings (--map (format "%s -- TSS %s" (car it) (cadr it))
                         tidied))
         (joined (s-join "\n" strings))
         (header "Cycling -- TSS per week (from cycling.org)")
         (underline (s-repeat (length header) "=")))
    (cd/string-to-special-buffer (s-join "\n" `(,header ,underline ,joined)) "*cycling-tss*")))

(defun cd/string-to-special-buffer (contents bufname)
  (interactive)
  (when (get-buffer-process "*cycling-tss*")
   (kill-buffer bufname))
  (get-buffer-create bufname)
  (switch-to-buffer-other-window bufname)
  (kill-region (point-min) (point-max))
  (insert contents)
  (special-mode)
  (evil-insert 1))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun read-file-to-string (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun erase-all-matches-from-start (regex)
  (replace-regexp regex "" nil (point-min) (point-max)))

;;; UNORGANISED
(defun zsh ()
  (interactive)
  (term "/usr/bin/zsh"))

(defun elpy-send-contiguous-block ()
  (interactive)
  (mark-paragraph)
  (elpy-shell-send-region-or-buffer)
  (evil-forward-paragraph))

;;; Emacs lisp
(defun eval-into-comment ()
  (interactive)
  (let ((sexp (elisp--preceding-sexp)))
    (save-excursion
      (goto-char (line-end-position))
      (delete-horizontal-space)
      (insert " ;; " (prin1-to-string (eval sexp))))))

(defun cd/heirarchical-category-drawer ()
  (interactive)
  (org-set-property "CATEGORY" (s-join "/" (s-split " " (read-string "Words: ")))))

(defun my-mark-as-project ()
  "This function makes sure that the current heading has
(1) the tag :project:
(2) has property COOKIE_DATA set to \"todo recursive\"
(3) has any TODO keyword and
(4) a leading progress indicator"
  (interactive)
  (org-set-property "COOKIE_DATA" "todo recursive")
  (org-back-to-heading t)
  (let* ((title (nth 4 (org-heading-components)))
         (keyword (nth 2 (org-heading-components))))
    (when (and (bound-and-true-p keyword) (string-prefix-p "[" title))
      (message "TODO keyword and progress indicator found"))
    (when (and (not (bound-and-true-p keyword)) (string-prefix-p "[" title))
      (message "no TODO keyword but progress indicator found")
      (forward-whitespace 1)
      (insert "TODO "))
    (when (and (not (bound-and-true-p keyword)) (not (string-prefix-p "[" title)))
      (message "no TODO keyword and no progress indicator found")
      (forward-whitespace 1)
      (insert "TODO [/] "))
    (when (and (bound-and-true-p keyword) (not (string-prefix-p "[" title)))
      (message "TODO keyword but no progress indicator found")
      (forward-whitespace 2)
      (insert "[/] ")))
  (org-toggle-tag "project" 'on))

(defun cd/goto-todays-cycling ()
  (interactive)
  (let* ((path (f-join org-directory "health-and-fitness" "cycling.org"))
         (thisyear (string-to-number (format-time-string "%Y")))
         (thisweek (string-to-number (format-time-string "%W")))
         (lastweek (if (eq thisweek 1) 52 (- thisweek 1)))
         (last-weeks-year (if (eq lastweek 52) (- thisyear 1) thisyear))
         (header (format "%4d W%2d" thisyear thisweek))
         (header-lastweek (format "%4d W%2d" last-weeks-year lastweek)))
    (find-file path)
    (+org/open-all-folds)
    (goto-char (point-min))
    (when (not (re-search-forward header nil t))
      (re-search-forward header-lastweek)
      (org-insert-heading)
      (yas-expand-snippet (yas-lookup-snippet "Week of Cycling Training")))
    (re-search-forward "^|") ;; Go to start of table
    (evil-beginning-of-line)
    (while  (s-matches? "^|" (thing-at-point 'line t)) ;; test first char on line == |
      (move-beginning-of-line 2))
    (previous-line)
    (org-narrow-to-subtree)))

(defun cd/get-keyword-key-value (kwd)
  (let ((data (cadr kwd)))
    (list (plist-get data :key)
          (plist-get data :value))))

(defun cd/org-current-buffer-get-title ()
  (cd/org-current-buffer-get-keyword-value "TITLE"))

(defun cd/org-current-buffer-get-keyword-value (keyword)
  (nth 1
       (assoc keyword
              (org-element-map (org-element-parse-buffer 'greater-element)
                  '(keyword)
                #'cd/get-keyword-key-value))))

(defun cd/org-file-get-keyword-value (file keyword)
  (with-current-buffer (find-file-noselect file)
    (cd/org-current-buffer-get-keyword-value keyword)))


(defun cd/org-file-get-title (file)
  (cd/org-file-get-keyword-value file "TITLE"))

(defun cd/projectile-magit-status ()
  "Jump to magit-status in a known projectile project."
  (interactive)
  (let ((project (completing-read "Project: "
                                  projectile-known-projects-on-file)))
    (magit-status project)))

(defun cd/org-table-sum-column (col)
  (interactive)
  (org-table-goto-line 2)
  (let ((total 0))
    (while (org-table-p)
      (setq total (+ total (let ((val (org-table-get nil col)))
                             (if val (string-to-number val) 0))))
      (next-line))
    total))

(defun cd/org-table-cycling-tss-sum ()
  (interactive)
  (message "Total TSS: %d" (cd/org-table-sum-column 4)))

(defun collect-duplicate-headings ()
  (let (dups contents hls)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward org-complex-heading-regexp nil t)
        (let* ((el (org-element-at-point))
               (hl (org-element-property :title el))
               (pos (org-element-property :begin el)))
          (push (cons hl pos) hls)))
      (setq contents
            (cl-loop for hl in hls
                     for pos = (goto-char (cdr hl))
                     for beg = (progn pos (line-beginning-position))
                     for end = (progn pos (org-end-of-subtree nil t))
                     for content = (buffer-substring-no-properties beg end)
                     collect (list (car hl) (cdr hl) content)))
      (dolist (elt contents)
        (when (> (cl-count (last elt) (mapcar #'last contents)
                           :test 'equal)
                 1)
          (push (cons (car elt)
                      (nth 1 elt))
                dups)))
      (nreverse dups))))

(defun show-duplicate-headings ()
  (interactive)
  (helm :sources (helm-build-sync-source "Duplicate headings"
                   :candidates (lambda ()
                                 (with-helm-current-buffer
                                   (collect-duplicate-headings)))
                   :follow 1
                   :action 'goto-char)))

(load-library "find-lisp")

(defun remove-org-mode-properties ()
  (interactive)
  (goto-char (point-min))
  (query-replace-regexp
   (rx bol (* " ") ":" (+ (any alnum "_")) ":" (* (seq " " (+ nonl))) "\n")
   ""))

(defun headercount (&optional level)
  (interactive)
  (save-excursion
    (let* ((stars (if level (s-repeat level "\*") "\*+"))
           (reg (concat "^" stars " "))
           (n-headers (count-matches reg (point-min) (point-max)))
           (level-str (if level (format " level ≤%d" level) ""))
           (msg (format "%d%s headers" n-headers level-str "headers")))
      (message msg))))

(defun insert-newline-if-not-at-start ()
  (unless (= (point) (line-beginning-position))
    (newline)))

(defun cd/point-of-first-header ()
  "Return the point of first org-mode-header, or nil if it doesn't exist."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\*" nil t)))

(defun cd/goto-end-of-toplevel-list ()
  "Find the first top-level list, or insert one if it doesn't exist."
  (interactive)
  (let ((pos-first-header (cd/point-of-first-header)))
    (goto-char (point-min))
    (if (re-search-forward "^-" (or pos-first-header (point-max)) t)
        (org-forward-paragraph)
      (if pos-first-header
          (progn
            (goto-char pos-first-header)
            (+evil/insert-newline-above 2)
            (evil-next-visual-line -2))
        (progn
          (org-forward-paragraph)
          (+evil/insert-newline-below 1)
          (evil-next-visual-line 1)
          nil)
        ))))

(defun cd/insert-in-toplevel-list (thing)
  (interactive)
  (save-excursion
    (if (cd/goto-end-of-toplevel-list)
        (+org/insert-item-below 1)
      (insert "-"))
    (evil-normal-state)
    (insert " " thing)))

(defun filename-to-pretty-title (filename)
  (s-capitalized-words
   (s-replace "-" " "
              (file-name-sans-extension (file-name-base filename)))))

(defun create-or-add-to-see-also-header (text)
  (save-excursion
    (unless (re-search-forward "^\* See Also" nil t)
      (goto-char (point-max))
      (evil-insert-newline-below)
      (insert "* See Also\n\n"))

    (org-narrow-to-subtree)
    (goto-char (point-max))
    (insert "- " text)
    (widen)))

(defun cd/org-roam-insert-to-see-also ()
  (interactive)
  (save-excursion
    (unless (re-search-forward "^\* See Also" nil t)
      (goto-char (point-max))
      (evil-insert-newline-below)
      (insert "* See Also\n\n"))

    (org-narrow-to-subtree)
    (goto-char (point-max))
    (insert "- ")
    (org-roam-insert)
    (widen)))


(defun org-file-from-subtree (&optional arg)
  "Take the current subtree and create a new file from
  it. Add a link at the top of the file in the first pre-header list.

In the new file, promote all direct children of the original
  subtree to be level 1-headings, and transform the original
  heading into the '#+TITLE' parameter.

If called with the universal argument, prompt for new filename,
otherwise use the subtree title.

With ARG, also visit the file.
"
  (interactive "P")
  (let* ((curdir (file-name-directory (buffer-file-name)))
         (filename (read-file-name "File: " curdir))
         (link (file-relative-name filename curdir))
         (title (filename-to-pretty-title filename))
         (link-text (format "[[file:%s][%s]]" link title))
         (curfile-relative-to-new (file-relative-name (buffer-file-name) (file-name-directory filename)))
         (curfile-title (filename-to-pretty-title buffer-file-name))
         (curfile-link (format "[[file:%s][%s]]" curfile-relative-to-new curfile-title)))
    ;; Copy current subtree into clipboard
    (org-cut-subtree)

    (save-excursion
      (create-or-add-to-see-also-header link-text)
      ;; (cd/insert-in-toplevel-list link-text)
      )
    (save-buffer)

    (with-temp-file filename
      (org-mode)
      (insert "#+TITLE: " title "\n\n")
      (org-paste-subtree)
      (create-or-add-to-see-also-header curfile-link))

    (when arg
      (find-file filename)))
  (org-roam-db-build-cache))

(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-find-file title nil nil 'no-confirm)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))


(defun org-file-from-selection (&optional clipboard-only)
  "Create a new file from current selection, inserting a link.

  Prompt for a filename, and create. Prompt for an org-mode
  TITLE, and insert. Insert the cut region. Then, insert the link
  into the source document, using TITLE as description"
  (interactive)
  (when (region-active-p)
    (let* ((filename (read-file-name "New filename: " org-directory))
           (file-relative (file-relative-name
                           filename
                           (file-name-directory (expand-file-name filename))))
           (title (read-from-minibuffer "Title: "))
           (link-text (format "[[file:%s][%s]]" link title)))
      (call-interactively' kill-region)
      (if clipboard-only
          (kill-new link-text)
        (save-excursion (cd/insert-in-toplevel-list link-text)))
      ;; (newline)
      (with-temp-file filename
        (org-mode)
        (insert (concat "#+TITLE: " title "\n\n"))
        (evil-paste-after 1)))))


(defun org-open-link-same-window ()
  (interactive)
  (let ((org-link-frame-setup '((file . find-file))))
    (org-open-at-point)))

(defun org-open-link-other-window ()
  (interactive)
  (let ((org-link-frame-setup '((file . find-file-other-window))))
    (org-open-at-point)))


(defun org-refile-to-file (&optional target level)
  (interactive)
  (let* ((filename (or target (ivy-read "Refile to: " (f-entries default-directory nil t))))
         (org-refile-targets `((,filename . (:maxlevel . ,(or level 3))))))
    (org-refile)))


(defun org-refile-to-this-file ()
  (interactive)
  (org-refile-to-file (buffer-name)))


(defun org-refile-to-this-file-level1 ()
  (interactive)
  (org-refile-to-file (buffer-name) 1))


(defun org-change-state-and-archive ()
  (interactive)
  (org-todo)
  (org-archive-subtree-default))


(defun org-paste-checkbox-list ()
  (interactive)
  (insert-newline-if-not-at-start)
  (insert (replace-regexp-in-string "^" "- [ ] " (current-kill 0))))


(defun org-paste-todo-header-list (&optional level)
  (interactive)
  (let* ((level (or level 1))
         (stars (s-repeat level "*"))
         (todo (s-concat stars " TODO ")))
    (insert-newline-if-not-at-start)
    (insert (replace-regexp-in-string "^" todo (current-kill 0)))))


(defun org-paste-todo-header-list-l2 ()
  (interactive)
  (org-paste-todo-header-list 2))


(defun org-paste-todo-header-list-l3 ()
  (interactive)
  (org-paste-todo-header-list 3))


(defun org-archive-level1-done ()
  (interactive)
  (save-excursion
    (goto-char 1)
    (+org/close-all-folds)
    (org-map-entries 'org-archive-subtree "/DONE" 'file)))


(defun org-copy-link-url (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))


(defun org-fix-blank-lines (prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))


(defun org-archive-file ()
  "Move current file into my org archive dir."
  (interactive)
  (let* ((archive-dir (f-join org-directory "archive"))
         (fname (file-name-nondirectory (buffer-file-name)))
         (new-fname (f-join archive-dir fname)))
    (rename-file (buffer-file-name) new-fname)))


(defun my-refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))

(defun org-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (org-fill-paragraph nil region)))

(defun find-todays-headline-or-create ()
  (interactive)
  (let* ((today-str (format-time-string "%Y-%m-%d %A"))
         (marker (org-find-exact-headline-in-buffer today-str)))
    (if marker (org-goto-marker-or-bmk marker)
      (progn (goto-char (point-max))
             (org-insert-heading)
             (insert " " today-str)))))


(defun org-update-all-checkbox-counts ()
  (interactive)
  (org-update-checkbox-count t))

(defun org-copy-link (&optional arg)
  "Copy org-mode links from anywhere within."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (raw-link (org-element-property :search-option link))
         (tidy (string-trim-left raw-link "\*")))
    (kill-new tidy)
    (message (concat "Copied Link: " tidy))))

(defun cd/org-copy-next-link ()
  "Find the next link, copy it to the kill ring, and leave the curser at the end."
  (interactive)
  (let* ((start (- (re-search-forward "\\[\\[") 2))
         (end (re-search-forward "\\]\\]")))
    (kill-ring-save start end)
    (goto-char end)))

(defun cd/org-files-under-dir (dir)
  (if (f-dir? dir)
      (find-lisp-find-files dir "\.org$")
    (find-lisp-find-files (f-join org-directory dir) "\.org$")))

(defun cd/do-and-archive ()
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(defun cd/kill-and-archive ()
  (interactive)
  (org-todo 'kill)
  (org-archive-subtree))

;; Visit every org file when emacs starts
(setq cd/preload-org-files nil)
(when cd/preload-org-files
  (dolist (it (org-agenda-files))
    (find-file-noselect it)))

(setq org-directory (f-join my-code-dir "knowledge")
      org-src-window-setup 'current-window
      org-indent-indentation-per-level 1
      org-adapt-indentation nil
      org-tags-column -60
      org-pretty-entities t
      org-catch-invisible-edits 'show-and-error
      org-imenu-depth 4
      ;; by default, open org links in SAME window
      org-link-frame-setup '((file . find-file))
      ;; org-link-frame-setup '((file . find-file-other-window))
      org-hide-emphasis-markers t
      org-todo-keywords '((sequence "TODO(t)"
                                    "NEXT(n)" ; PRIORITISED todo
                                    "BLCK(b)" ; CANNOT DO JUST NOW
                                    "WIP(w)"
                                    "|"
                                    "DONE(d)"
                                    "KILL(k)" ; WON'T DO
                                    ))
      org-cycle-separator-lines 0
      org-list-indent-offset 2
      org-modules nil
      org-treat-insert-todo-heading-as-state-change t
      org-log-repeat 'time
      org-log-done 'time
      org-log-done-with-time nil
      org-log-into-drawer t
      org-archive-location (f-join org-directory "archive/%s_archive::")
      org-refile-use-outline-path 't
      org-refile-allow-creating-parent-nodes 'confirm
      org-startup-folded 'fold
      org-id-track-globally t
      org-image-actual-width 600
      org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
      org-superstar-headline-bullets-list '("➤" "⇒" "⇛" "⤍" "⤏" "⤑"))

;; Org download (+dragndrop)
(setq org-download-method 'directory)
(setq org-download-image-dir '(lambda () (interactive) (get-relative-asset-dir)))

;; Babel
(setq org-babel-python-command "~/.envs/py/bin/python3")

;; Deft
(setq deft-directory org-directory)
(setq deft-recursive t)

(defun cd/org-roam--title-to-slug (title)
  "Convert TITLE to a filename-suitable slug."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]/]" . "-")  ;; convert anything not alphanumeric
                    ("\-\-*" . "-")  ;; remove sequential underscores
                    ("^\-" . "")  ;; remove starting underscore
                    ("\-$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (downcase slug))))

(setq org-roam-directory (f-join my-code-dir "knowledge"))
(setq +org-roam-open-buffer-on-find-file t)
(setq org-roam-rename-file-on-title-change nil)
(setq org-roam-tag-sources '(prop all-directories))
;; (setq org-roam-tag-sources '(prop))
(setq org-roam-title-to-slug-function 'cd/org-roam--title-to-slug)
(setq org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point "%?"
                                    :file-name "${slug}"
                                    :head "#+title: ${title}\n"
                                    :unnarrowed t)))
(setq org-roam-buffer-width 0.25)

;;; org-capture for literature
(defun read-capitalized-title ()
  (s-titleize (read-string "Title: ")))

(defun read-author ()
  (let ((name (read-string "Author: " "" nil nil)))
    (if (s-equals? name "")
        nil
      (format-author-name name))))

(defun format-author-name (author)
  (concat (seq-mapcat
           (lambda (author-part)
             (if (> (length author-part) 1)
                 (s-concat " " (s-capitalize author-part))
               (s-concat (s-capitalize author-part) ".")))
           (s-split " " author))))

(defun maybe-get-bibtex ()
  "Maybe get a DOI number for a reference"
  (let ((doi (read-string "DOI: " "" nil nil)))
    (if (s-equals? doi "")
        nil
      (s-concat ("\n")))))

(defun read-authors ()
  (let ((authors (read-author))
        (running t))
    (while running
      (let ((input (read-author)))
        (if (s-equals? input nil)
            (setq running nil)
          (setq authors (concat authors " and " input)))))
    authors))

(defun emoji-heading (fontfunc fonticon headingname)
  (let ((icon (funcall fontfunc fonticon :face 'all-the-icons-purple :v-adjust 0.01)))
    (format "%s %s" icon headingname)))

(defun faicon-heading (icon msg)
  (emoji-heading 'all-the-icons-faicon icon msg))

(defun octicon-heading (icon msg)
  (emoji-heading 'all-the-icons-octicon icon msg))

(defun cd/org-datetree-find-dayonly-create ()
  (goto-char (point-min))
  (let* ((date (org-read-date nil t))
         (yyyy (format-time-string "%Y" date))
         (mm (format-time-string "%m" date))
         (dd (format-time-string "%d" date))
         (ddnum (string-to-number dd))
         (re (format "^\\* %s-%s-\\([0123][0-9]\\) \\w+$" yyyy mm))
         (datestr (format-time-string "%Y-%m-%d %a" date)))

    ;; Search for the same year-month, while we're still finding dates
    ;; within this month that are earlier than our target date.
    (while (and (setq match (re-search-forward re nil t))
                (goto-char (match-beginning 1))
                (< (string-to-number (match-string 1)) ddnum)))

    (cond
     (;; 
      (not match)
      (+org/insert-item-below 1)
      (insert datestr "\n")
      (previous-line)
      (evil-normal-state))
     (;; We've found a headline with the same date
      (= (string-to-number (match-string 1)) (string-to-number dd))
      (goto-char (point-at-bol))
      )
     (t
      (beginning-of-line)
      (+org/insert-item-above 1)
      (insert datestr "\n")
      (previous-line)
      (evil-normal-state)
      )
     )
    ))

(defun cd/org-file-today (subdir)
  (f-join org-directory subdir (format-time-string "%Y-%m-%d.org")))

(defun cd/org-file-future (subdir)
  (let* ((future (org-read-date)))
    (setq cd/last-future-date future)
    (f-join org-directory subdir (concat future ".org"))))

(defun cd/insert-or-make-org-link ()
  "If the clipboard is a url, ask for a title. Otherwise, assume an org-link."
  (let ((clip (current-kill 0)))
    (if (s-starts-with? "http" clip)
        (concat "[[" clip "][" (read-string "Title: ") "]]")
      clip)))

(setq org-capture-templates
      (doct `(

              ;;   ("todo" :keys "t"
              ;;  :file "todo.org" :template "* TODO %?")

              ;; ("todo [WORK]" :keys "w"
              ;;  :file "work.org" :olp ("Admin") :template "* TODO %?")

              ;; ("todo [CYBELE]" :keys "c"
              ;;  :file "work.org" :olp ("Research" "CYBELE")
              ;;  :template "* TODO %?")

              ;; ("research" :keys "r"
              ;;  :file "todo.org" :headline "RESEARCH"
              ;;  :template "* TODO %?")

              ;; ("journal" :keys "j"
              ;;  :file "journal.org" :function cd/org-datetree-find-dayonly-create
              ;;  :template "* %?")
              ("inbox" :keys "i"
               :file "inbox.org"
               :type entry
               :template "* %<=%F %H:%M=> %?")

              ("interstitial journal" :keys "I"
               :file "~/.interstitial-journal.org"
               :type item
               :template "- %U %?")

              ;; ("journal TODO" :keys "J"
              ;;  :file "journal.org" :function cd/org-datetree-find-dayonly-create
              ;;  :template "* TODO %?")

              ("logbook" :keys "l"
               :file "logbook.org" :function cd/org-datetree-find-dayonly-create
               :template "* %?")

              ;; ("logbook TODO" :keys "L"
              ;;  :file "logbook.org" :function cd/org-datetree-find-dayonly-create
              ;;  :template "* TODO %?")

              ;; ("URL" :keys "u"
              ;;  :file "todo.org" :headline "Bookmarks"
              ;;  :immediate-finish t
              ;;  :template "* TODO %(cd/insert-or-make-org-link)")

              ;; ("Literature" :keys "L"
              ;;  :file "literature.org" :headline "REFILE"
              ;;  :type entry
              ;;  :immediate-finish t
              ;;  :template "* TODO %(read-capitalized-title)\n\n%(read-authors)")

              ;; ("Korean" :keys "k"
              ;;  :file "language-learning.org" :olp ("Korean" "Vocabulary to find")
              ;;  :type checkitem :template "[ ] %?")
              )))

(map! "<f1>" '(lambda () (interactive) (org-capture nil "i"))
      "<f2>" '(lambda () (interactive) (org-capture nil "l"))
      "<f3>" 'org-roam-insert
      "<f4>" 'cd/org-roam-insert-to-see-also
      )
;; (map! "<f1>" 'org-capture
;;       "<f2>" 'org-agenda
;;       "<f3>" '(lambda () (interactive) (org-agenda nil "co") (goto-char (point-min)))
;;       "<f4>" '(lambda () (interactive) (org-agenda nil "cr") (goto-char (point-min))))

;;; Org AGENDA
(setq org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      ;; inhibit-startup nil means that if we want files to start 'folded', then agenda
      ;; will respect this
      ;; inhibit-startup t means 'just unfold', and can greatly speed up agenda
      ;; if there are many folded headings
      org-agenda-inhibit-startup t
      org-agenda-dim-blocked-tasks nil
      org-agenda-ignore-drawer-properties '(effort appt)
      org-agenda-show-all-dates t ; nil hides days in agenda if no tasks on that day
      ;; org-agenda-files (--filter (not (s-matches? "archive\\|recipes\\|thought" it))
      ;;                            (find-lisp-find-files org-directory "\.org$"))
      ;; All the files in the root of org directory
      org-agenda-files (append `(,org-directory)
                               ;; ...and any non-dotted directory underneath it
                               (--filter (and (f-directory-p (f-join org-directory it))
                                              (not (s-matches? (rx bol (+ ".")) it))
                                              (not (s-matches? "archive" it))
                                              (not (s-matches? "book-notes" it)))
                                         (directory-files org-directory)))
      ;; (--filter (not (s-matches? "archive\\|recipes\\|thought" it))
      ;;                            (find-lisp-find-files org-directory "\.org$"))
      org-agenda-file-regexp "\\`[^.].*\\.org\\'"
      org-refile-targets `((org-agenda-files . (:maxlevel . 2)))
      org-agenda-span 'week
      org-agenda-start-day nil
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-scheduled-if-done nil
      org-agenda-skip-deadline-if-done nil
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-skip-archived-trees nil
      org-agenda-block-separator ""
      org-agenda-compact-blocks nil
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-sort-notime-is-late nil
      org-agenda-remove-tags t
      org-agenda-time-grid '((daily today require-timed remove-match)
                             (800 1000 1200 1400 1600 1800 2000)
                             "......"
                             "")
      org-agenda-use-time-grid t
      org-agenda-prefix-format '((agenda . "%-20c%-12t%6s")
                                 (timeline . "% s")
                                 (todo . "%-20c")
                                 (tags . "%-20c")
                                 (search . "%-20c"))
      org-agenda-deadline-leaders '("!!! " "D%-2d " "D-%-2d ")
      org-agenda-scheduled-leaders '("" "S-%-2d ")
      org-agenda-sorting-strategy '((agenda time-up todo-state-up  category-up  scheduled-down priority-down)
                                    (todo todo-state-down category-up priority-down)
                                    (tags priority-down category-keep)
                                    (search category-keep))
      )

;;; Org AGENDA
(setq org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      ;; inhibit-startup nil means that if we want files to start 'folded', then agenda
      ;; will respect this
      ;; inhibit-startup t means 'just unfold', and can greatly speed up agenda
      ;; if there are many folded headings
      org-agenda-inhibit-startup t
      org-agenda-dim-blocked-tasks nil
      org-agenda-ignore-drawer-properties '(effort appt)
      org-agenda-show-all-dates t ; nil hides days in agenda if no tasks on that day
      ;; org-agenda-files (--filter (not (s-matches? "archive\\|recipes\\|thought" it))
      ;;                            (find-lisp-find-files org-directory "\.org$"))
      ;; All the files in the root of org directory
      org-agenda-files (append `(,org-directory)
                               ;; ...and any non-dotted directory underneath it
                               (--filter (and (f-directory-p (f-join org-directory it))
                                              (not (s-matches? (rx bol (+ ".")) it))
                                              (not (s-matches? "archive" it))
                                              (not (s-matches? "book-notes" it)))
                                         (directory-files org-directory)))
      ;; (--filter (not (s-matches? "archive\\|recipes\\|thought" it))
      ;;                            (find-lisp-find-files org-directory "\.org$"))
      org-agenda-file-regexp "\\`[^.].*\\.org\\'"
      org-refile-targets `((org-agenda-files . (:maxlevel . 2)))
      org-agenda-span 'week
      org-agenda-start-day nil
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-scheduled-if-done nil
      org-agenda-skip-deadline-if-done nil
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-skip-archived-trees nil
      org-agenda-block-separator ""
      org-agenda-compact-blocks nil
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-sort-notime-is-late nil
      org-agenda-remove-tags t
      org-agenda-time-grid '((daily today require-timed remove-match)
                             (800 1000 1200 1400 1600 1800 2000)
                             "......"
                             "")
      org-agenda-use-time-grid t
      org-agenda-prefix-format '((agenda . "%-20c%-12t%6s")
                                 (timeline . "% s")
                                 (todo . "%-20c")
                                 (tags . "%-20c")
                                 (search . "%-20c"))
      org-agenda-deadline-leaders '("!!! " "D%-2d " "D-%-2d ")
      org-agenda-scheduled-leaders '("" "S-%-2d ")
      org-agenda-sorting-strategy '((agenda time-up todo-state-up  category-up  scheduled-down priority-down)
                                    (todo todo-state-down category-up priority-down)
                                    (tags priority-down category-keep)
                                    (search category-keep))
      )
(defun f-org (filename)
  "Filename relative to my org directory."
  (f-join org-directory filename))

(defun cd/work-files ()
  (-map 'f-org '("work.org" "logbook.org" "literature.org")))

(defun cd/reading-files ()
  (append (cd/org-files-under-dir "book-notes")
          `(,(f-org "reading.org"))))

(defun cd/non-work-files ()
  (let* ((non-work (cl-set-difference (org-agenda-files) (cd/work-files) :test 'equal)))
    non-work))

(defun cd/literature-files ()
  `(,(f-org "literature.org")))

(defun cd/non-reading-files ()
  (--filter (not (s-matches? "reading\\|literature" it))
            (org-agenda-files)))

;;; Org AGENDA
(setq org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      ;; inhibit-startup nil means that if we want files to start 'folded', then agenda
      ;; will respect this
      ;; inhibit-startup t means 'just unfold', and can greatly speed up agenda
      ;; if there are many folded headings
      org-agenda-inhibit-startup t
      org-agenda-dim-blocked-tasks nil
      org-agenda-ignore-drawer-properties '(effort appt)
      org-agenda-show-all-dates t ; nil hides days in agenda if no tasks on that day
      ;; org-agenda-files (--filter (not (s-matches? "archive\\|recipes\\|thought" it))
      ;;                            (find-lisp-find-files org-directory "\.org$"))
      ;; All the files in the root of org directory
      org-agenda-files (append `(,org-directory)
                               ;; ...and any non-dotted directory underneath it
                               (--filter (and (f-directory-p (f-join org-directory it))
                                              (not (s-matches? (rx bol (+ ".")) it))
                                              (not (s-matches? "archive" it))
                                              (not (s-matches? "book-notes" it)))
                                         (directory-files org-directory)))
      ;; (--filter (not (s-matches? "archive\\|recipes\\|thought" it))
      ;;                            (find-lisp-find-files org-directory "\.org$"))
      org-agenda-file-regexp "\\`[^.].*\\.org\\'"
      org-refile-targets `((org-agenda-files . (:maxlevel . 2)))
      org-agenda-span 'week
      org-agenda-start-day nil
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-scheduled-if-done nil
      org-agenda-skip-deadline-if-done nil
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-skip-archived-trees nil
      org-agenda-block-separator ""
      org-agenda-compact-blocks nil
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-sort-notime-is-late nil
      org-agenda-remove-tags t
      org-agenda-time-grid '((daily today require-timed remove-match)
                             (800 1000 1200 1400 1600 1800 2000)
                             "......"
                             "")
      org-agenda-use-time-grid t
      org-agenda-prefix-format '((agenda . "%-20c%-12t%6s")
                                 (timeline . "% s")
                                 (todo . "%-20c")
                                 (tags . "%-20c")
                                 (search . "%-20c"))
      org-agenda-deadline-leaders '("!!! " "D%-2d " "D-%-2d ")
      org-agenda-scheduled-leaders '("" "S-%-2d ")
      org-agenda-sorting-strategy '((agenda time-up todo-state-up  category-up  scheduled-down priority-down)
                                    (todo todo-state-down category-up priority-down)
                                    (tags priority-down category-keep)
                                    (search category-keep))
      )
(defun agenda-header (msg)
  (let* ((char       (nth 2 '("╌" "-" " " "=")))
         (borderchar (nth 3 '("╌" "-" " " "=")))
         (n-tokens (/ (- 80 2 1 (length msg)) 2))
         (token-str (s-repeat n-tokens char))
         (extra (s-repeat (mod n-tokens 2) char))
         (spaced-str (format "%s%s  %s  %s" token-str extra msg token-str))
         (border (s-repeat (length spaced-str) borderchar)))
    (s-join "\n" `(,border ,spaced-str ,border))))

(setq org-agenda-custom-commands
      `(("c" . "Custom agenda views")

        ("co" "Overview Agenda"
         ((agenda "" ((org-agenda-overriding-header (agenda-header "TODAY"))
                      (org-agenda-span 1)
                      (org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-start-day "-0d")))

          ;; show a todo list of IN-PROGRESS
          (todo "WIP|NEXT" ((org-agenda-overriding-header (agenda-header "In Progress -- Work"))
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-files (cl-set-difference (cd/work-files)
                                                                 (cd/literature-files)
                                                                 :test 'equal))))
          (todo "WIP|NEXT" ((org-agenda-overriding-header (agenda-header "In Progress -- Personal"))
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-files (cd/non-work-files))))

          (todo "BLCK" ((org-agenda-overriding-header (agenda-header "BLOCKED"))))
          ))

        ("cw" "Work tasks"
         ((todo "BLCK" ((org-agenda-overriding-header (agenda-header "BLOCKED"))
                        (org-agenda-files (cl-set-difference (cd/work-files)
                                                             (cd/literature-files)
                                                             :test 'equal))))

          ;; show a todo list of IN-PROGRESS
          (todo "WIP|NEXT" ((org-agenda-overriding-header (agenda-header "In Progress"))
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-files (cl-set-difference (cd/work-files)
                                                                 (cd/literature-files)
                                                                 :test 'equal))))
          (todo "TODO" ((org-agenda-overriding-header (agenda-header "Todo"))
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-files (cl-set-difference (cd/work-files)
                                                             (cd/literature-files)
                                                             :test 'equal))))))

        ("cr" "Review the last week"
         ((agenda "" ((org-agenda-start-day "-7d")
                      (org-agenda-entry-types '(:timestamp))
                      (org-agenda-archives-mode t)
                      (org-agenda-later 1)
                      (org-agenda-log-mode 16)
                      (org-agenda-log-mode-items '(closed clock state))
                      (org-agenda-show-log t)))))

        ("cR" "Reading -- in progress, and possible future books"
         ((todo ""
                ((org-agenda-files (cd/reading-files))
                 (org-agenda-overriding-header (cd/text-header "Books in Progress" nil t))))
          (todo ""
                ((org-agenda-files (cd/literature-files))
                 (org-agenda-overriding-header (cd/text-header "Literature in Progress" nil t))))))
        ))

(defun cd/refile-to-top-level ()
  (interactive)
  (let ((org-refile-use-outline-path 'file)
        (org-refile-targets `((org-agenda-files . (:level . 0)))))
    (org-refile)))

;;; Org HOOKS
(add-hook! org-mode
           'visual-line-mode
           '(lambda () (interactive) (setq fill-column 120))
           #'visual-fill-column-mode
           'org-indent-mode
           'abbrev-mode
           ;; 'mixed-pitch-mode
           'undo-tree-mode
           '(lambda () (set-face-italic 'italic t)))
;; (remove-hook! org-agenda-mode '(lambda () (interactive) (goto-char (point-min))))
(add-hook! 'auto-save-hook 'org-save-all-org-buffers)

(after! org
  (add-to-list 'org-structure-template-alist '("p" . "src python")))

(setq tramp-default-method "sshx")
(setq my-remote-servers
      '(("skye" :username "cdavison" :ip "130.159.94.19")
        ("uist" :username "cdavison" :ip "130.159.95.176" :hop "skye")
        ("cava" :username "cdavison" :ip "130.159.94.251" :hop "skye")
        ("bute" :username "cdavison" :ip "130.159.94.204" :hop "skye")
        ("jura" :username "cdavison" :ip "130.159.94.214" :hop "skye")
        ("iona" :username "cdavison" :ip "130.159.94.187" :hop "skye")))


(defun cd/extract-ssh-connection (&optional name)
  (if (boundp 'my-remote-servers)
      ;; my-remote-servers should be a plist of (SERVER :username USER :ip IP)
      (let* ((selected (if name name (completing-read "Server: " (mapcar 'car my-remote-servers) nil t)))
             (data (cdr (assoc selected my-remote-servers)))
             (username (plist-get data :username))
             (ip (plist-get data :ip))
             (hop (plist-get data :hop)))
        `(,username ,ip ,hop))
    ;; otherwise, read a username and an ip
    (let ((username (read-string "Username: "))
          (ip (read-string "ip: "))
          (hop nil))
      `(,username ,ip ,hop))))

(defun connect-remote ()
  (interactive)
  (let* ((data (cd/extract-ssh-connection))
         (username (car data))
         (folder (if (string= username "root") "/" (format "/home/%s/" username)))
         (ip (car (cdr data)))
         (hop (car (cdr (cdr data))))
         (hopdata (if hop (cd/extract-ssh-connection hop) nil))
         (hopstr (if hopdata (format "sshx:%s@%s|"
                                     (car hopdata)
                                     (car (cdr hopdata)))
                   ""))
         (connstr (format "sshx:%s@%s" username ip))
         (conn (format "/%s%s:%s" hopstr connstr folder)))
    (dired conn)))

(setq theme-preferences-light '(
                                doom-opera-light
                                doom-solarized-light
                                doom-plain
                                ))

(setq theme-preferences-dark '(
                               doom-monokai-pro
                                doom-dracula
                                doom-monokai-classic
                               doom-horizon
                               doom-plain-dark
                               ))

(setq doom-theme (nth 0 theme-preferences-dark))

(defun theme-toggle-light-dark ()
  (interactive)
  (if (cl-position doom-theme theme-preferences-light)
      (set-theme-dark)
    (set-theme-light)))

(defun set-theme-dark ()
  (interactive)
  (setq doom-theme (nth 0 theme-preferences-dark))
  (doom/reload-theme))

(defun set-theme-light ()
  (interactive)
  (setq doom-theme (nth 0 theme-preferences-light))
  (doom/reload-theme))

(defun choose-pretty-theme (&optional subset)
  "Set a theme from one of the available fonts that I like"
  (interactive)
  (let* ((themes (pcase subset
                   ('light theme-preferences-light)
                   ('dark theme-preferences-dark)
                   (_ (append theme-preferences-light theme-preferences-dark))))
         (choice (ivy-read "Pick theme:" themes)))
    (setq doom-theme (intern choice))
    (doom/reload-theme)))

(defun choose-pretty-light-theme ()
  (interactive)
  (choose-pretty-theme 'light))

(defun choose-pretty-dark-theme ()
  (interactive)
  (choose-pretty-theme 'dark))


(defun next-theme (&optional backward alternate-theme-list)
  (interactive)
  (let* ((themes (if alternate-theme-list alternate-theme-list (custom-available-themes)))
         (idx-current (cl-position doom-theme themes))
         (idx-next (next-circular-index (if idx-current idx-current 0) (length themes) (if backward t nil)))
         (next (nth idx-next themes)))
    (setq doom-theme next)
    (doom/reload-theme)
    (message "%s" next)
    ))

(defun next-theme-dark ()
  (interactive)
  (next-theme nil theme-preferences-dark))

(defun next-theme-light ()
  (interactive)
  (next-theme nil theme-preferences-light))

(setq cd-fonts (--filter (member it (font-family-list))
                         '(
                           "Monego"
                           ;; "Ubuntu Mono"
                           ;; "Anonymous Pro"
                           ;; "Iosevka Term"
                           ;; "Fira Mono"
                           ;; "Rec Mono Linear"
                           ;; "Rec Mono SemiCasual"
                           "Hack"
                           "Inconsolata"
                           "Source Code Pro"
                           ;; "Fantasque Sans Mono"
                           ;; "CamingoCode"
                           "Roboto Mono"
                           ;; "Liberation Mono"
                           )))

(setq cd-mixed-pitch-fonts (--filter (member it (font-family-list))
                                     '(
                                       "Karla"
                                       "Lato"
                                       "Ubuntu"
                                       "Helvetica"
                                       "Monaco"
                                       "Montserrat"
                                       )))

(setq cd/font-size "-14")
(when cd-fonts
  (setq doom-font (concat (nth 0 cd-fonts) cd/font-size)))

(when cd-mixed-pitch-fonts
  (setq doom-variable-pitch-font (concat (nth 0 cd-mixed-pitch-fonts) cd/font-size)))

(defun set-pretty-font ()
  "Set a font from one of the available fonts that I like"
  (interactive)
  (setq doom-font (ivy-read "Pick font:" cd-fonts))
  (doom/reload-font))

(defun next-font ()
  (interactive)
  (let* ((pos (cl-position (car (s-split "-" doom-font)) cd-fonts :test 's-equals?))
         (next-pos (% (+ 1 pos) (length cd-fonts)))
         (next-font-name (nth next-pos cd-fonts)))
    (set-frame-font next-font-name 1)
    (setq doom-font (concat next-font-name "-14"))
    (message next-font-name)))

(setq fullscreen-at-startup t)
(when fullscreen-at-startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq split-width-threshold 150)

(map! "C-<" 'avy-goto-word-1) ;; C-S-,

(map! :n "C-;" 'iedit-mode
      :n "C-:" 'iedit-mode-toggle-on-function)

(map! "M-%" 'anzu-query-replace
      "C-M-%" 'anzu-query-replace-regexp)

(map! :leader
      :desc "Toggle light/dark theme" "t t" 'theme-toggle-light-dark
      :desc "<<here>>" "j h" 'jump-to-here-anchor
      :desc "[t]odos" "j t" '(lambda () (interactive) (find-file (f-join my-code-dir "knowledge" "todo.org")))
      :desc "[w]ork" "j w" '(lambda () (interactive) (find-file (f-join my-code-dir "knowledge" "work.org")))
      :desc "[s]cratch" "j s" '(lambda () (interactive) (find-file "~/scratch/scratch.org"))
      :desc "[j]ournal" "j j" '(lambda () (interactive) (org-capture-goto-target "j"))
      :desc "[l]ogbook" "j l" '(lambda () (interactive) (org-capture-goto-target "l"))
      :desc "last [c]apture" "j c" '(lambda () (interactive) (org-capture-goto-last-stored))
      :desc "todays [C]ycling" "j C" 'cd/goto-todays-cycling
      :desc "[b]ookmarks" "j b" '(lambda () (interactive) (org-capture-goto-target "u")))

(map! :leader
      (:prefix-map ("a" . "applications")
       (:prefix ("r" . "repoutil")
        :desc "Status of all branches" "b" #'repoutil-branchstat
        :desc "Fetch all branches" "f" #'repoutil-fetch
        :desc "List all managed repos" "l" #'repoutil-list
        :desc "List all unclean repos" "u" #'repoutil-unclean)
       (:prefix ("g" . "ripgrep")
        :desc "org notes" "o" 'rg-org
        :desc "logbook" "l" 'rg-logbook)
       (:prefix ("d" . "downloader")
        :desc "quick add" "q" 'cd/nas/quick-add-download
        :desc "list" "l" 'cd/nas/list-downloads)
       (:prefix ("j" . "jump to notes")
        :desc "index notes" "i" 'cd/find-index-file
        :desc "thought notes" "t" 'cd/find-thought-file
        :desc "book lists" "b" 'cd/find-book-list-file
        )
       ("n" 'new-in-git)
       ("i" 'cd/what-was-I-doing)
       ("I" (lambda () (interactive) (org-capture nil "I")))
       )
      (:prefix-map ("T" . "tagsearch")
       :desc "List tags in this dir" "l" 'tagsearch-list
       :desc "Files with specific tags" "f" '(lambda () (interactive)
                                               (files-matching-tagsearch
                                                (read-string "Tags: ")
                                                default-directory))
       :desc "ORG Files with specific tags" "o" '(lambda () (interactive)
                                               (files-matching-tagsearch
                                                (read-string "Tags: ")
                                                org-directory))

       ))

(map! "<f5>" 'find-previous-file
      "<f6>" 'find-next-file
      "C-<left>" 'find-previous-file
      "C-<right>" 'find-next-file)

(map! "<f7>" 'next-narrow
      "<f8>" '(lambda () (interactive) (next-narrow 'back)))

(map! "<f9>" 'er/expand-region)

(map! "M-<left>" 'winner-undo
      "M-<right>" 'winner-redo)

;; Emacs capture and org-mode
(map! :map org-mode-map :leader :n
      "m r a" 'org-change-state-and-archive
      "m r A" 'org-archive-to-archive-sibling
      "m r D" 'cd/do-and-archive
      "m r K" 'cd/kill-and-archive
      "m r t" 'org-refile-to-this-file
      "m r T" 'org-refile-to-this-file-level1
      "m r F" 'cd/refile-to-top-level
      "m d i" 'org-time-stamp-inactive
      "m h" 'headercount
      "o s" 'org-open-link-same-window
      "o O" 'org-open-link-other-window
      "o o" 'org-open-at-point
      "o S" 'org-sidebar-toggle
      "Q" 'org-unfill-paragraph
      "N" 'org-toggle-narrow-to-subtree
      "n R" 'helm-org-rifle
      "m l u" 'org-copy-link-url
      "m l C" 'cd/org-copy-next-link)

(map! :map org-mode-map :n
      "C-x C-n" 'org-file-from-subtree
      ;; "C-x C-n" 'org-roam-create-note-from-headline
      :v "C-x C-n" 'org-file-from-selection)

(map! :map dired-mode-map :n "/" 'dired-narrow)

(map! :nv "j" 'evil-next-visual-line
      :nv "k" 'evil-previous-visual-line)

(map! :leader
      :prefix "w"
      :desc "evil-window-split (follow)" "s"
      (lambda () (interactive) (evil-window-split) (evil-window-down 1))
      :desc "evil-window-vsplit (follow)" "v"
      (lambda () (interactive) (evil-window-vsplit) (evil-window-right 1)))



(map! :after projectile :leader
      :desc "Find Org-dir file (no archive)" "<SPC>"
      'org-roam-find-file
      :desc "Find Org-dir file" "S-<SPC>"
      '(lambda () (interactive) (projectile-find-file-in-directory org-directory)
      ))

(map! :map haskell-mode-map
      "C-x C-e" 'haskell-process-load-file)

(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "win32yank.exe -i")
  (deactivate-mark))

(defun wsl-paste ()
  (interactive)
  (let ((clipboard
         (shell-command-to-string "win32yank.exe -o")))
    (insert (substring (replace-regexp-in-string "\r" "" clipboard) 0 -1))))

(defun wsl_interop ()
  (interactive)
  (setq is-wsl? nil)
  (when (string-match ".*microsoft.*" (shell-command-to-string "uname -a"))
    (setenv "WSL_INTEROP" (string-trim (shell-command-to-string "cat ~/.wsl_interop")))
    (setq is-wsl? t
          browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
          browse-url-generic-args '("/c" "start")
          browse-url-browser-function #'browse-url-generic
          x-selection-timeout 10)))

(shell-command "wsl_interop_setup")
(wsl_interop)

(when is-wsl?
  (cd my-code-dir))

(add-hook! dired-mode #'dired-hide-dotfiles-mode)
(setq pdf-info-epdfinfo-program "/usr/bin/epdfinfo")

(rg-enable-menu)

(setq calendar-week-start-day 1)

(define-minor-mode dired-follow-mode
  "Diplay file at point in dired after a move."
  :lighter " dired-f"
  :global t
  (if dired-follow-mode
      (advice-add 'dired-next-line :after (lambda (arg) (dired-display-file) (org-roam-update)))
    (advice-remove 'dired-next-line (lambda (arg) (dired-display-file) (org-roam-update)))))

(defun cd/what-was-I-doing ()
  "Show the last interstitial journal item."
  (interactive)
  (let* ((bufname "*interstitial-journal*")
         (fname (expand-file-name "~/.interstitial-journal.org"))
         (contents (s-split "\n" (read-file-to-string fname)))
         (matching (--filter  (s-matches? "^- " it) contents))
         (last-item (car (last matching)))
         (parts (s-split " " last-item))
         (timestamp (s-join " " (-slice parts 1 4)))
         (text (s-concat "    " (s-join " " (-slice parts 4))))
         (title "Interstitial Journal")
         (underline (s-repeat (length title) "="))
         (msg (s-join "\n" `(,title ,underline "" ,(substring timestamp 1 (- (length timestamp) 1)) ,text))))
    (cd/string-to-special-buffer msg bufname)))
(set-popup-rule! "^\\*interstitial-journal*" :side 'bottom :size 0.30 :select t :ttl 1)

(org-roam-mode t)
