(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 120
        whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode)

