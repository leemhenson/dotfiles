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

(global-hl-line-mode)
