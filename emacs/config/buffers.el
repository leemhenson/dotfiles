(use-package buffer-move
  :ensure t
  :bind
  ("C-S-<up>" . buf-move-up)
  ("C-S-<down>" . buf-move-down)
  ("C-S-<left>" . buf-move-left)
  ("C-S-<right>" . buf-move-right))

(use-package eyebrowse
  :ensure t
  :commands (
    eyebrowse-create-window-config
    eyebrowse-last-window-config
    eyebrowse-next-window-config
    eyebrowse-prev-window-config
    eyebrowse-rename-window-config
    eyebrowse-switch-to-window-config)
  :init
  (eyebrowse-mode)
  (evil-leader/set-key
    "wc" 'eyebrowse-create-window-config
    "wl" 'eyebrowse-last-window-config
    "wn" 'eyebrowse-next-window-config
    "wp" 'eyebrowse-prev-window-config
    "wr" 'eyebrowse-rename-window-config
    "ww" 'eyebrowse-switch-to-window-config))

(use-package transpose-frame
  :ensure t
  :commands (transpose-frame
             rotate-frame-clockwise)
  :init
  (evil-leader/set-key
    "wtf" 'transpose-frame
    "wtr" 'rotate-frame-clockwise))

(use-package unkillable-scratch
  :ensure t)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))))

(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)
