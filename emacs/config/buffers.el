(use-package buffer-move
  :ensure t
  :bind
  ("C-S-<up>" . buf-move-up)
  ("C-S-<down>" . buf-move-down)
  ("C-S-<left>" . buf-move-left)
  ("C-S-<right>" . buf-move-right))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))))

(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

