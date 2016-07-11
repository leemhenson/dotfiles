(use-package planet-theme
  :ensure t)

(use-package noctilux-theme
  :ensure t
  :config
  (load-theme 'noctilux t))

(set-face-attribute 'default nil
                    :height 140
                    :family "Inconsolata")
