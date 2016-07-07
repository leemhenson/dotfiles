(use-package hl-anything
  :ensure t
  :diminish 'hl-paren-mode
  :config
  (add-hook 'prog-mode-hook #'hl-paren-mode))

(use-package hl-todo
  :ensure t
  :diminish 'global-hl-todo-mode
  :config
  (global-hl-todo-mode))

(global-hl-line-mode)
