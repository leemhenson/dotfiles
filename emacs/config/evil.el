(defun split-window-right-and-balance ()
  (interactive)
  (split-window-right)
  (balance-windows))

(use-package evil
  :ensure t
  :init

  (use-package evil-leader ;; must be loaded before evil
    :ensure t
    :config
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode)
    (evil-leader/set-key
      "!"   'shell-command
      "bk"  'kill-this-buffer
      "bK"  'kill-other-buffers
      "wk"  'delete-window
      "wsb" 'split-window-below
      "wsr" 'split-window-right-and-balance))

  :config
  (evil-mode)

  ;; http://eugeii.com/posts/bypassing-the-clipboard-in-emacs-evil-mode/

  (defmacro without-evil-mode (&rest do-this)
    ;; Check if evil-mode is on, and disable it temporarily
    `(let ((evil-mode-is-on (evil-mode?)))
       (if evil-mode-is-on
           (disable-evil-mode))
       (ignore-errors
         ,@do-this)
       (if evil-mode-is-on
           (enable-evil-mode))))

  (defmacro evil-mode? ()
    "Checks if evil-mode is active. Uses Evil's state to check."
    `evil-state)

  (defmacro disable-evil-mode ()
    "Disable evil-mode with visual cues."
    `(progn (evil-mode 0)))

  (defmacro enable-evil-mode ()
    "Enable evil-mode with visual cues."
    `(progn (evil-mode 1)))

  (defun evil-destroy-paste-before ()
    (interactive)
    (without-evil-mode
     (delete-region (point) (mark))
     (evil-paste-before 1)))

  (defun evil-destroy-paste-after ()
    (interactive)
    (without-evil-mode
     (delete-region (point) (mark))
     (evil-paste-after 1)))

  (define-key evil-visual-state-map "P" 'evil-destroy-paste-before)
  (define-key evil-visual-state-map "p" 'evil-destroy-paste-after)

  (use-package evil-nerd-commenter
    :ensure t
    :commands (evilnc-comment-or-uncomment-lines
               comment-or-uncomment-region)
    :init
    (evil-leader/set-key
      "ci" 'evilnc-comment-or-uncomment-lines
      "cr" 'comment-or-uncomment-region))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-terminal-cursor-changer
    :ensure t
    :init
    (unless (display-graphic-p)
      (evil-terminal-cursor-changer-activate)
      (setq evil-motion-state-cursor 'box)  ; █
      (setq evil-visual-state-cursor 'box)  ; █
      (setq evil-normal-state-cursor 'box)  ; █
      (setq evil-insert-state-cursor 'bar)  ; ⎸
      (setq evil-emacs-state-cursor  'hbar) ; _
      ))

  (use-package evil-visualstar
    :ensure t
    :config
    (global-evil-visualstar-mode))

  (define-key evil-visual-state-map
    (kbd "C-o")
    (concat ":m '>+1" (kbd "RET") "gv=gv"))

  (define-key evil-visual-state-map
    (kbd "C-p")
    (concat ":m '<-2" (kbd "RET") "gv=gv")))
