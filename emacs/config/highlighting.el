(use-package hl-anything
  :ensure t
  :diminish 'hl-paren-mode
  :config
  (add-hook 'prog-mode-hook #'hl-paren-mode)
  (setq hl-inward-paren-bg-color "#192129")
  (setq hl-inward-paren-fg-color "LimeGreen")
  (setq hl-outward-paren-bg-colors '("#192129"))
  (setq hl-outward-paren-fg-colors '("gold"
                                     "chocolate"
                                     "SteelBlue1"
                                     "MistyRose"
                                     "MediumPurple")))

(use-package hl-todo
  :ensure t
  :diminish 'global-hl-todo-mode
  :config
  (global-hl-todo-mode))

(global-hl-line-mode)
