(use-package avy
  :ensure t
  :commands avy-goto-char-timer
  :init
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
    "sb" 'ivy-switch-buffer
    "sf" 'counsel-find-file
    "sg" 'counsel-git
    "sx" 'counsel-M-x
    "sy" 'counsel-yank-pop
    "hb" 'counsel-descbinds
    "hf" 'counsel-describe-function))

