;; Theming.
;; (use-package base16-theme
;;   :ensure t)

;; (use-package gruber-darker-theme
;;   :ensure t)

;; (use-package noctilux-theme
;;   :ensure t)

;; (use-package soothe-theme
;;   :ensure t)

(use-package planet-theme
  :ensure t
  :config
  (load-theme 'planet t)
  (setq hl-inward-paren-bg-color 'planet-bg)
  (setq hl-inward-paren-fg-color "LimeGreen")
  (setq hl-outward-paren-bg-colors '(planet-bg))
  (setq hl-outward-paren-fg-colors '("gold"
                                     "chocolate"
                                     "SteelBlue1"
                                     "MistyRose"
                                     "MediumPurple"))
  )

(set-face-attribute 'default nil
                    :height 140
                    :family "Inconsolata")
