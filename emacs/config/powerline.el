(use-package powerline
  :ensure t
  :init
  (setq powerline-default-separator 'nil)
  :config
  (use-package powerline-evil
    :ensure t
    :config
    (powerline-evil-vim-color-theme)))

