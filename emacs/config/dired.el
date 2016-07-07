(use-package dired-single
  :ensure t
  :config
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map "-"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

(use-package diredful
  :ensure t
  :config
  (diredful-mode))

(define-key evil-normal-state-map (kbd "-") 'dired-jump)
(define-key dired-mode-map "c" 'find-file)

