;;; ../code/dotfiles/.doom.d/autoload/fonts.el -*- lexical-binding: t; -*-

(setq font-preferences
      '("Dank Mono" "Hack" "Rec Mono Casual" "Rec Mono Linear" "Rec Mono SemiCasual"
        "Inconsolata" "JetBrains Mono" "Source Code Pro" "Cascadia Code"
        "Fantasque Sans Mono" "CamingoCode" "Roboto Mono" "Ubuntu Mono"
        "Liberation Mono" "Fira Code" "Iosevka Term"))

(setq cd-fonts (--filter (member it (font-family-list)) font-preferences))

(defvar current-font-idx 0)

(defun set-pretty-font ()
  "Set a font from one of the available fonts that I like"
  (interactive)
  (set-frame-font (ivy-read "Pick font:" cd-fonts) 1))

(defun next-font ()
  (interactive)
  (setq current-font-idx
        (% (+ 1 current-font-idx)
           (length cd-fonts)))
  (let ((next-font-name (nth current-font-idx cd-fonts)))
    (set-frame-font next-font-name 1)
    (message next-font-name)))