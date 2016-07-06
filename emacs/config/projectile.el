(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (evil-leader/set-key
    "pd" 'projectile-dired
    "st" 'projectile-ag)
  (use-package counsel-projectile
    :ensure t
    :commands counsel-projectile
    :init
    (evil-leader/set-key "sp" 'counsel-projectile)))

