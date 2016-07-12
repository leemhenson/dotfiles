(use-package planet-theme
  :ensure t)

(use-package noctilux-theme
  :ensure t
  :config
  (load-theme 'noctilux t))

(set-face-attribute 'default nil
                    :height 120
                    :family "InconsolataDzfp")

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (load-theme 'smart-mode-line-respectful t))
