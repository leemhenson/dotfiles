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
      "wsr" 'split-window-right))
  :config
  (evil-mode)
  (use-package evil-nerd-commenter
    :ensure t
    :commands (evilnc-comment-or-uncomment-lines
               comment-or-uncomment-region)
    :init
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
