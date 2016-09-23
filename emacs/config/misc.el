(global-linum-mode t)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq gc-cons-threshold (* 10 1024 1024)) ;; Only GC every 10MB
(setq load-prefer-newer t) ;; Always load newest byte code
(setq large-file-warning-threshold 100000000) ;; Warn when opening files bigger than 100MB
(setq inhibit-startup-screen t)
(setq require-final-newline t)
(setq backup-directory-alist '(("~/.emacs.d/backups")))
(setq create-lockfiles nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Make is possible for me to enter a # symbol via Alt-3
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
