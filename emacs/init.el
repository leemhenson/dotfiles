;; Cached start time for benchmarking emacs boot time

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst emacs-start-time (current-time))

;; Bootstrap 
(require 'cl-lib)

(defun build-config-path (file)
  (locate-user-emacs-file
    (concat
      (concat "config/" file)
      ".el")))

(defun load-config-file (file)
  (load (build-config-path file)))

(load-config-file "misc")
(load-config-file "package")
(load-config-file "theme")

;; custom-set-variables, custom-safe-themes, custom-set-faces etc
(setq custom-file (build-config-path "custom"))
(load custom-file)

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
      (global-evil-surround-mode))
    (use-package evil-visualstar
      :ensure t
      :config
      (global-evil-visualstar-mode)))
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
    "sx" 'counsel-M-x
    "sy" 'counsel-yank-pop
    "hb" 'counsel-descbinds
    "hf" 'counsel-describe-function))

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

(load-config-file "javascript")

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

(use-package smooth-scrolling
  :ensure t
  :diminish smooth-scrolling-mode
  :config
  (smooth-scrolling-mode)
  (setq smooth-scroll-margin 25))

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

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode)

(use-package yasnippet
  :ensure t
  ;; :commands yas-minor-mode
  ;; :diminish yas-minor-mode
  ;; :init
  ;; (progn (add-hook 'prog-mode-hook #'yas-minor-mode))
  :config
  (setq yas-snippet-dirs '("/Users/leemhenson/.dotfiles/emacs/snippets"))
  (progn (yas-reload-all)))

(use-package powerline
  :ensure t
  :init
  (setq powerline-default-separator 'nil)
  :config
  (use-package powerline-evil
    :ensure t
    :config
    (powerline-evil-vim-color-theme)))

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
(define-key dired-mode-map "c" 'find-file)

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

