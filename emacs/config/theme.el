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
  (load-theme 'planet t))

(set-face-attribute 'default nil
                    :height 140
                    :family "Inconsolata")

(global-hl-line-mode +1)

