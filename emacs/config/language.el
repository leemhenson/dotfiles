(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode)
         ("\\.jsx$" . js2-jsx-mode))
  :commands (js2-mode
             js2-jsx-mode)
  :config
  (defvar sgml-basic-offset)
  (defvar sgml-attribute-offset)
  (setq js-indent-level 2)
  (setq js2-basic-offset js-indent-level
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil
        sgml-basic-offset js-indent-level
        sgml-attribute-offset js-indent-level))

(use-package json-mode
  :ensure t
  :mode (("\\.json$" . json-mode))
  :commands (json-mode))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :commands lua-mode)

(use-package flycheck
  :ensure t
  :mode (("\\.js$" . js2-mode)
         ("\\.jsx$" . js2-mode))
  :config
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'json-mode-hook 'flycheck-mode))
