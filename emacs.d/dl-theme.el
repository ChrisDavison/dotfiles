;;; dl-theme.el :: Minimalist, light theme for emacs

;; Copyright (C) 2015 by Chris Davison

;; Author: Chris Davison <c.jr.davison@gmail.com>
;; URL: 
;; Package-Version: 20150714.001
;; Version: 0.01
;; License: MIT

;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;;; COLOURS:
;; highlight-bg "#DCE1F2"
;; link blue "#65ADF5" 
;; purple 0 "#483A58"
;; purple 1 "#8681C6"
;; purple 2 "#928CE9"
;; purple 3 "#B79CED"
;; peach pink "#F76F8E"
(setq dl-purple0 "#483A58")

;;; Code:


(deftheme dl
  "davison-light theme")

(custom-theme-set-faces
 'dl

 '(default ((t (:background "#ffffff" :foreground "#000000"))))
 '(region ((t (:background "#F76F8E" :foreground "#000"))))

 '(bold ((t (:weight bold))))
 '(bold-italic ((t (:weight bold :slant italic))))
 '(italic ((t (:slant italic))))
 '(underline ((t (:underline t))))

 ;; '(css-selector ((t (:foreground "#ffc0de"))))
 ;; '(css-property ((t (:foreground "#66D9EF"))))

 ;; '(diff-added ((t (:foreground "#FFBD98" :weight bold))))
 ;; '(diff-context ((t (:foreground "#F8F8F2"))))
 ;; '(diff-file-header ((t (:foreground "#66D9EF" :background nil))))
 ;; '(diff-indicator-added ((t (:foreground "#FFBD98"))))
 ;; '(diff-indicator-removed ((t (:foreground "#ffc0de"))))
 ;; '(diff-header ((t (:foreground "#F8F8F2" :background "#232526"))))
 ;; '(diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
 ;; '(diff-removed ((t (:foreground "#ffc0de" :weight bold))))

 '(escape-glyph ((t (:foreground "#F76F8E"))))

 '(minibuffer-prompt ((t (:foreground "#B79CED" :weight bold))))

 '(mode-line ((t (:foreground "#111" :background "#eee"
                              :box (:line-width 1 :color "#eee" :style released-button)))))
 '(mode-line-buffer-id ((t (:foreground "#111" :background "#eee" :weight semi-bold))))
 '(mode-line-inactive ((t (:foreground "#bbb" :background "#eee"
                                       ':box (:line-width 1 :color "#232526")))))
 '(mode-line-mousable ((t (:foreground "#111" :background "#eee"))))
 '(mode-line-mousable-minor-mode ((t (:foreground "#111" :background "#eee"))))

 '(font-lock-builtin-face ((t (:foreground "#555"))))
 '(font-lock-comment-face ((t (:foreground "#aaa" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#aaa" :weight bold))))
 '(font-lock-constant-face ((t (:foreground "#222" :weight bold))))
 '(font-lock-doc-face ((t (:foreground "#928CE9" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "#222" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "#555" :weight bold))))
 '(font-lock-negation-char-face ((t (:weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "#222" :weight bold))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold))))
 '(font-lock-string-face ((t (:foreground "#928CE9"))))
 '(font-lock-type-face ((t (:foreground "#555"))))
 '(font-lock-variable-name-face ((t (:foreground "#555"))))
 '(font-lock-warning-face ((t (:foreground "#FF0000" ':background "#333333"))))
 
 '(fringe ((t (:background "#eee"))))
 '(highlight ((t (:foreground "#000" :background  "#DCE1F2"))))
 '(hl-line ((t (:foreground "#000" :background "#DCE1F2"))))

 ;; '(isearch ((t (:foreground "#C4BE89" :background "#000000"))))
 ;; '(isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))

 ;; '(lazy-highlight ((t (:foreground "#465457" :background "#000000"))))

 '(markdown-italic-face ((t (:slant italic))))
 '(markdown-bold-face ((t (:weight bold))))
 '(markdown-header-face ((t (:weight normal))))
 '(markdown-header-face-1 ((t (:foreground "#483A58"))))
 '(markdown-header-face-2 ((t (:foreground "#8681C6"))))
 '(markdown-header-face-3 ((t (:foreground "#928CE9"))))
 '(markdown-header-face-4 ((t (:foreground "#B79CED"))))
 ;; '(markdown-inline-code-face ((t (:foreground "#66D9EF"))))
 ;; '(markdown-list-face ((t (:foreground "#FFBD98"))))
 ;; '(markdown-blockquote-face ((t (:slant italic))))
 ;; '(markdown-pre-face ((t (:foreground "#AE81FF"))))
 ;; '(markdown-link-face ((t (:foreground "#66D9EF"))))
 ;; '(markdown-reference-face ((t (:foreground "#66D9EF"))))
 ;; '(markdown-url-face ((t (:foreground "#E6DB74"))))
 ;; '(markdown-link-title-face ((t (:foreground "#ffc0de"))))
 ;; '(markdown-comment-face ((t (:foreground "#465457"))))
 ;; '(markdown-math-face ((t (:foreground "#AE81FF" :slant italic))))

 '(org-hide ((t (:foreground "#ffffff"))))
 '(outline-1 ((t (:foreground "#483A58" :weight bold))))
 '(outline-2 ((t (:foreground "#8681C6"))))
 '(outline-3 ((t (:foreground "#928CE9"))))
 '(outline-4 ((t (:foreground "#B79CED"))))
 
 '(helm-source-header ((t (:background "#F76F8E" :foreground "#fff"))))
 '(helm-selection ((t (:background "#F76F8E" :foreground "#d9d9d9"))))
 '(helm-visible-mark ((t (:background "#F76F8E" :foreground "#d9d9d9"))))

 '(link ((t (:foreground "#ff00ff" :underline t))))

 '(secondary-selection ((t (:background "#4b4b4b"))))
 ;; '(show-paren-match-face ((t (:foreground "#000000" :background "#FD971F"))))
 ;; '(show-paren-mismatch-face ((t (:foreground "#960050" :background "#1E0010"))))
 )

;;; autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dl)

;;; dl.el ends here
