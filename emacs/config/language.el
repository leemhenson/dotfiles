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

;; This defun can be removed when the patch it includes is added to emacs core.
;; https://github.com/mooz/js2-mode/issues/369
(defun js--multi-line-declaration-indentation ()
  (let (forward-sexp-function ; use Lisp version even in js2-mode
        at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at js--declaration-keyword-re))
        (when (looking-at js--indent-operator-re)
          (goto-char (match-end 0)))
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (js--backward-syntactic-ws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2
                                     (skip-syntax-backward ".")
                                     (looking-at js--indent-operator-re)
                                   (js--backward-syntactic-ws))
                                 (not (eq (char-before) ?\;)))
                            (js--same-line pos)))))
          (condition-case nil
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at js--declaration-keyword-re)
          (goto-char (match-end 0))
          (1+ (current-column)))))))

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

(use-package parinfer
  :ensure t
  :init
  (progn
    (setq parinfer-extensions
          '(defaults
            pretty-parens
            evil)))
  (add-hook 'emacs-lisp-mode-hook #'parinfer-mode))

(use-package php-mode
  :ensure t
  :mode ("\\.php$" . php-mode)
  :commands php-mode
  :config
  (setq-default tab-width 2)
  (setq-default c-basic-offset 2))

(use-package scss-mode
  :ensure t
  :mode (("\\.scss$" . scss-mode))
  :commands (scss-mode)
  :config
  (setq-default css-indent-offset 2))


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
  :mode (("\\.html$" . web-mode)
         ("\\.liquid$" . web-mode))
  :commands (web-mode)
  :config
  (setq-default web-mode-markup-indent-offset 2))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml$" . yaml-mode)
         ("``.yml$" . yaml-mode))
  :commands (yaml-mode))

(defun find-eslint-for-flycheck ()
  "If ESLint found in node_modules directory - use that for flycheck."
  (interactive)
  (let ((ext (downcase (concat "" (file-name-extension buffer-file-name))))
        (executable "node_modules/.bin/eslint"))
    (when (string= ext "js")
      (let ((local-eslint-dir (locate-dominating-file buffer-file-name executable)))
        (when local-eslint-dir
          (let ((local-eslint (concat local-eslint-dir executable)))
            (setq flycheck-javascript-eslint-executable local-eslint)))))))

(defun find-eslint-config-for-flycheck ()
  "If dominating .eslintrc.json found - use that for flycheck."
  (interactive)
  (let ((ext (downcase (concat "" (file-name-extension buffer-file-name))))
        (config-file ".eslintrc.json"))
    (when (string= ext "js")
      (let ((local-eslintrc-dir (locate-dominating-file buffer-file-name config-file)))
        (when local-eslintrc-dir
          (setq flycheck-eslint-rules-directories (list (expand-file-name local-eslintrc-dir))))))))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'find-file-hook 'find-eslint-config-for-flycheck)
  (add-hook 'find-file-hook 'find-eslint-for-flycheck))

(use-package flycheck-flow
  :ensure t
  :config
  (setq flycheck-javascript-flow-args "--respect-pragma")
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-setup)

(add-to-list 'package-archives
             '("emacs-pe" . "https://emacs-pe.github.io/packages/"))

(use-package purescript-mode
  :ensure t
  :mode (("\\.purs$" . purescript-mode))
  :commands purescript-mode
  :pin emacs-pe)

(use-package psc-ide
  :ensure t
  :after purescript-mode
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation))))

(use-package flycheck-purescript
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(flycheck-purescript-setup)))

(use-package graphql-mode
  :ensure t
  :mode (("\\.graphql$" . graphql-mode))
  :commands graphql-mode)
