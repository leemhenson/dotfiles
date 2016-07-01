;; Initialize package system.
;; Use :package-refresh-contents if local package cache seems to be out of date.
(defconst emacs-start-time (current-time))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Miscellaneous.
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

;; Bootstrap 'use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Make use-package available.
(defvar use-package-verbose t)
(require 'use-package)

;; Theming.
(use-package base16-theme :ensure t :defer t)
(load-theme 'base16-twilight-dark t)
(global-hl-line-mode +1)

;; Load these packages before other stuff
(use-package diminish
  :ensure t)

(use-package evil
  :ensure t
  :init
  (use-package evil-leader ;; must be loaded before evil
    :ensure t
    :config
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode)
    (evil-leader/set-key
      "!"   'shell-command
      "bk"  'kill-this-buffer
      "wk"  'delete-window
      "wsb" 'split-window-below
      "wsr" 'split-window-right)
    (use-package evil-nerd-commenter
      :ensure t
      :config
      (evil-leader/set-key
        "ci" 'evilnc-comment-or-uncomment-lines
        "cr" 'comment-or-uncomment-region))
    (use-package evil-surround
      :ensure t
      :config
      (global-evil-surround-mode)))
  :config
  (evil-mode))

;; Other packages
(use-package ag
  :ensure t)

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (ac-config-default))

(use-package avy
  :ensure t
  :commands avy-goto-char-timer
  :init
  (evil-leader/set-key
    "<SPC>" 'avy-goto-char-timer))

(use-package counsel
  :ensure t
  :commands (ivy-switch-buffer
             counsel-find-file
             counsel-git
             counsel-descbinds
             counsel-describe-function
             counsel-M-x)
  :init
  (evil-leader/set-key
    "bs" 'ivy-switch-buffer
    "sf" 'counsel-find-file
    "sg" 'counsel-git
    "sy" 'counsel-yank-pop
    "hb" 'counsel-descbinds
    "hf" 'counsel-describe-function
    "hx" 'counsel-M-x))

(use-package dired-details
  :ensure t
  :config
  (setq-default dired-details-hidden-string "")
  (dired-details-install))

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments '("-c"))
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :mode (("\\.js$" . js2-mode)
         ("\\.jsx$" . js2-mode))
  :config
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'json-mode-hook 'flycheck-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (global-highlight-parentheses-mode))

(use-package hl-todo
  :ensure t
  :diminish 'global-hl-todo-mode
  :config
  (global-hl-todo-mode))

(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode)
         ("\\.jsx$" . js2-mode))
  :commands (js2-mode)
  :config
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-trailing-comma-warning nil))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (evil-leader/set-key
    "pd" 'projectile-dired
    "st" 'projectile-ag)
  (use-package counsel-projectile
    :ensure t
    :commands counsel-projectile
    :init
    (evil-leader/set-key "sp" 'counsel-projectile)))

(use-package json-mode
  :ensure t
  :mode (("\\.json$" . json-mode))
  :commands (json-mode))

(use-package magit
  :ensure t
  :commands magit-status
  :init
  (evil-leader/set-key "gs" 'magit-status)
  :config
  (set-face-background 'magit-diff-added "#001800")
  (set-face-foreground 'magit-diff-added "#00FF00")
  (set-face-background 'magit-diff-added-highlight "#000000")
  (set-face-foreground 'magit-diff-added-highlight "#00FF00")
  (set-face-background 'magit-diff-removed "#180000")
  (set-face-foreground 'magit-diff-removed "#FF0000")
  (set-face-background 'magit-diff-removed-highlight "#000000")
  (set-face-foreground 'magit-diff-removed-highlight "#FF0000"))

(use-package pbcopy
  :ensure t
  :config
  (turn-on-pbcopy))

(use-package smooth-scrolling
  :ensure t
  :diminish smooth-scrolling-mode
  :config
  (smooth-scrolling-mode)
  (setq smooth-scroll-margin 25))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (use-package window-numbering
    :ensure t
    :config
    (defun window-numbering-install-mode-line (&optional position)
      "Do nothing, the display is handled by spaceline.")
    (window-numbering-mode)
    (evil-leader/set-key
      "1" 'select-window-1
      "2" 'select-window-2
      "3" 'select-window-3
      "4" 'select-window-4
      "5" 'select-window-5
      "6" 'select-window-6
      "7" 'select-window-7
      "8" 'select-window-8
      "9" 'select-window-9)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 120
        whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(defun kill-other-buffers ()
  "Kill all buffers but the current one. Doesn't mess with special buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
    (seq-each
     #'kill-buffer
     (delete (current-buffer) (seq-filter #'buffer-file-name (buffer-list))))))

(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

(define-key evil-normal-state-map (kbd "-") 'dired-jump)

(define-key evil-visual-state-map
  (kbd "C-o")
  (concat ":m '>+1" (kbd "RET") "gv=gv"))

(define-key evil-visual-state-map
  (kbd "C-p")
  (concat ":m '<-2" (kbd "RET") "gv=gv"))

(advice-add 'evil-scroll-page-down :after (lambda (&rest args) (recenter)))
(advice-add 'evil-scroll-page-up :after (lambda (&rest args) (recenter)))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (window-numbering spaceline smooth-scrolling pbcopy magit json-mode js2-mode hl-todo highlight-parentheses flycheck exec-path-from-shell dired-details desktop+ counsel avy auto-complete ag evil-surround evil-nerd-commenter evil-leader evil base16-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
