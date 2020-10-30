;;; ../code/dotfiles/.doom.d/+rust.el -*- lexical-binding: t; -*-

;;; Programming - rust
(add-hook! rust-mode
           '(company-mode
             flycheck-rust-setup
             cargo-minor-mode
             racer-mode
             (lambda () (add-to-list 'company-backends 'company-racer))))
(add-hook! racer-mode '(company-mode eldoc-mode))
(add-to-list 'auto-mode-alist '("\\.rs" . rust-mode))
