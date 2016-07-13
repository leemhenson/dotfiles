(use-package planet-theme
  :ensure t)

(set-face-attribute 'default nil
                    :height 120
                    :family "InconsolataDzfp")

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  (sml/setup))
