;;; ../code/dotfiles/.doom.d/+appearance.el -*- lexical-binding: t; -*-

;; -----------------------------------------------------------------------------
;;; APPEARANCE (font and theme)
;; -----------------------------------------------------------------------------
(setq theme-preferences-light '(apropospriate-light
                                doom-plain)
      theme-preferences-dark '(doom-dracula
                               doom-sourcerer
                               doom-old-hope
                               doom-palenight
                               kaolin-temple
                               kaolin-bubblegum
                               doom-one))

(setq doom-theme (nth 0 theme-preferences-dark))

(setq fullscreen-at-startup t)
(when fullscreen-at-startup
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq cd-fonts (--filter (member it (font-family-list))
                         '("Rec Mono SemiCasual"
                           "Hack"
                           "Rec Mono Linear"
                           "Inconsolata"
                           "Source Code Pro"
                           "Fantasque Sans Mono"
                           "CamingoCode"
                           "Roboto Mono"
                           "Liberation Mono"
                           "Iosevka Term")))

(setq cd/font-size 14
      doom-font (format "%s-%d" (nth 0 cd-fonts) cd/font-size)
      doom-variable-pitch-font (format "%s-%d" "Montserrat" cd/font-size))

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

(setq split-width-threshold 100)
