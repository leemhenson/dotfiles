(use-package avy
  :ensure t
  :commands avy-goto-char-timer
  :init
  (setq avy-timeout-seconds 0.8)
  (evil-leader/set-key
    "<SPC>" 'avy-goto-char-timer))

(use-package counsel
  :ensure t
  :commands (ivy-switch-buffer
             counsel-find-file
             counsel-git
             counsel-descbinds
             counsel-describe-function
             counsel-M-x)
  :init
  (evil-leader/set-key
    "hb" 'counsel-descbinds
    "hf" 'counsel-describe-function
    "mx" 'counsel-M-x
    "sb" 'ivy-switch-buffer
    "sf" 'counsel-find-file
    "sg" 'counsel-git
    "sy" 'counsel-yank-pop
    ))

