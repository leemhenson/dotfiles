(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("/Users/leemhenson/.dotfiles/emacs/snippets"))
  (progn (yas-reload-all))
  (yas-global-mode))
