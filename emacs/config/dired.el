(use-package dired-details
  :ensure t
  :config
  (setq-default dired-details-hidden-string "")
  (dired-details-install))

(define-key evil-normal-state-map (kbd "-") 'dired-jump)
(define-key dired-mode-map "c" 'find-file)

