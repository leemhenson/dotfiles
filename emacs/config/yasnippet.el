(use-package yasnippet
  :ensure t
  ;; :commands yas-minor-mode
  ;; :diminish yas-minor-mode
  ;; :init
  ;; (progn (add-hook 'prog-mode-hook #'yas-minor-mode))
  :config
  (setq yas-snippet-dirs '("/Users/leemhenson/.dotfiles/emacs/snippets"))
  (progn (yas-reload-all)))
