(use-package anzu
  :ensure t
  :diminish global-anzu-mode
  :config
  (global-anzu-mode))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))))

;; (defun kill-other-buffers ()
;;   "Kill all buffers but the current one. Doesn't mess with special buffers."
;;   (interactive)
;;   (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
;;     (seq-each
;;      #'kill-buffer
;;      (delete (current-buffer) (seq-filter #'buffer-file-name (buffer-list))))))

(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)
