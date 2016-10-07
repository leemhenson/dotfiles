(use-package avy
  :ensure t
  :commands avy-goto-char-timer
  :init
  (setq avy-timeout-seconds 0.8)
  (evil-leader/set-key
    "<SPC>" 'avy-goto-char-timer))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (evil-leader/set-key
    "pd" 'projectile-dired
    "pa" 'projectile-ag))

(use-package counsel
  :ensure t
  :commands (ivy-switch-buffer
             counsel-ag
             counsel-find-file
             counsel-git
             counsel-descbinds
             counsel-describe-function
             counsel-M-x
             swiper)
  :init
  (defun counsel-ag-advice (args)
    "Make counsel-ag aware of project root directory,
modify INITIAL-DIRECTORY to projectile-project-root."
    (setcar (nthcdr 1 args) projectile-project-root))

  (advice-add 'counsel-ag :filter-args #'counsel-ag-advice)

  (evil-leader/set-key
    "ca" 'counsel-ag
    "cb" 'ivy-switch-buffer
    "cd" 'counsel-descbinds
    "cD" 'counsel-describe-function
    "cf" 'counsel-git
    "cF" 'counsel-find-file
    "cy" 'counsel-yank-pop
    "cx" 'counsel-M-x
    "s"  'swiper
    ))

(use-package counsel-projectile
  :ensure t
  :commands counsel-projectile
  :init
  (evil-leader/set-key "cp" 'counsel-projectile-switch-project))
