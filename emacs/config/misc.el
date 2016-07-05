(global-linum-mode t)
(menu-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq gc-cons-threshold (* 10 1024 1024)) ;; Only GC every 10MB
(setq load-prefer-newer t) ;; Always load newest byte code
(setq large-file-warning-threshold 100000000) ;; Warn when opening files bigger than 100MB
(setq inhibit-startup-screen t)
(setq require-final-newline t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
