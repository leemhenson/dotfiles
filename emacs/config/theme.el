;; (use-package subatomic-theme
;;   :ensure t)

;; (use-package sublime-themes
;;   :ensure t)

;; (use-package flatland-theme
;;   :ensure t)

;; (use-package gruber-darker-theme
;;   :ensure t)

(use-package ujelly-theme
  :ensure t)

;; (use-package afternoon-theme
;;   :ensure t)

;; (use-package planet-theme
;;   :ensure t)

(set-face-attribute 'default nil
                    :height 120
                    :family "InconsolataDzfp")

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  (sml/setup)
  (column-number-mode))
