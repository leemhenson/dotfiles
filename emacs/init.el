;; Cached start time for benchmarking emacs boot time

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst emacs-start-time (current-time))

;; Bootstrap 
(require 'cl-lib)

(defun build-config-path (file)
  (locate-user-emacs-file
    (concat
      (concat "config/" file)
      ".el")))

(defun load-config-file (file)
  (load (build-config-path file)))

(load-config-file "misc")
(load-config-file "package")
(load-config-file "theme")
(load-config-file "evil")
(load-config-file "osx")
(load-config-file "swiper")

(load-config-file "ag")
(load-config-file "auto-complete")
(load-config-file "buffers")
(load-config-file "dired")
(load-config-file "highlighting")
(load-config-file "language")
(load-config-file "magit")
(load-config-file "projectile")
(load-config-file "scrolling")
(load-config-file "undo")
(load-config-file "whitespace")
(load-config-file "yasnippet")

;; custom-set-variables, custom-safe-themes, custom-set-faces etc
(setq custom-file (build-config-path "custom"))
(load custom-file)

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)

