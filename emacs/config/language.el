(use-package haskell-mode
  :ensure t
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs" . haskell-mode))
  :commands (haskell-mode)
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode)
         ("\\.jsx$" . js2-jsx-mode))
  :commands (js2-mode
             js2-jsx-mode)
  :init
  (setq-default js2-bounce-indent-p t)
  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset js-indent-level
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil
        sgml-basic-offset js-indent-level
        sgml-attribute-offset js-indent-level)
  (defvar sgml-basic-offset)
  (defvar sgml-attribute-offset)
  :config
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'js2-jsx-mode-hook 'flycheck-mode))

(use-package json-mode
  :ensure t
  :mode (("\\.json$" . json-mode))
  :commands (json-mode)
  :config
  (setq-default js-indent-level 2)
  (setq-default json-reformat:indent-width 2)
  (add-hook 'json-mode-hook 'flycheck-mode))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :commands lua-mode
  :config
  (setq-default lua-indent-level 2))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md$" . gfm-mode)
         ("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode))
  :commands (markdown-mode
             gfm-mode)
  :init
  (setq-default markdown-command "multimarkdown"))

(use-package php-mode
  :ensure t
  :mode ("\\.php$\\'" . php-mode)
  :commands php-mode
  :config
  (setq-default tab-width 2)
  (setq-default c-basic-offset 2))

(use-package scss-mode
  :ensure t
  :mode (("\\.scss$" . scss-mode))
  :commands (scss-mode))

(use-package sh-script
  :ensure t
  :init
  (setq-default sh-indentation 2
                sh-basic-offset 2))

(use-package slim-mode
  :ensure t
  :mode ("\\.slim$" . slim-mode)
  :commands slim-mode)

(use-package terraform-mode
  :ensure t
  :mode (("\\.tf$" . terraform-mode))
  :commands (terraform-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :commands (web-mode)
  :config
  (setq-default web-mode-markup-indent-offset 2))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml$" . yaml-mode)
         ("``.yml$" . yaml-mode))
  :commands (yaml-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode)

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-setup)
