;;; webdev.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Special module for configuring html/css and javascript + any html templating langauges
;;; Code:
(require 'webdev-funcs)
(require 'webdev-keybinds)

(use-package web-mode
  :config
  (setq web-mode-enable-auto-closing t
	web-mode-enable-auto-opening t
	web-mode-enable-auto-indentation t))

(use-package js2-mode
  :mode "\\.m?js\\'"
  :interpreter "node"
  :commands js2-line-break
  :after projectile
  :config
  (setq js2-skip-preprocessor-directives t
	js-chain-indent t
	;; let flycheck do this
	j2-mode-show-parse-errors nil
	js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-highlight-external-variables t)
    (add-to-list 'projectile-project-root-files "package.json")
    (add-to-list 'projectile-globally-ignored-directories "node_modules"))


(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; setup file extensions for web mode
(setq auto-mode-alist (append auto-mode-alist '(("\\.html.eex\\'" . web-mode))))

(print auto-mode-alist)

;; disable default flycheck jslint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)


(use-package add-node-modules-path)

;; add local eslint for project
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

(provide 'webdev)
;;; webdev.el ends here
