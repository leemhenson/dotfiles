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

