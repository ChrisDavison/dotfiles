;; -----------------------------------------------------------------------------
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; -----------------------------------------------------------------------------

(require 'dash)  ;; Stuff like map, each, filter
(require 'f)  ;; Filepath functions
(require 's)  ;; String functions
(require 'rx) ;; Literate regular expressions

(require 'org)
(after! org
  (org-babel-load-file (f-join "~/.doom.d" "literate-config.org")))
