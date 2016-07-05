;; Initialize package system.
;; Use :package-refresh-contents if local package cache seems to be out of date.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap 'use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Make use-package available.
(defvar use-package-verbose t)
(require 'use-package)
