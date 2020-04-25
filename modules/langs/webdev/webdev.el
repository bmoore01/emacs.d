;;; webdev.el --- Special module for configuring html/css and javascript + any html templating langauges
;;; Commentary:
;;; Code:
(require 'webdev-funcs)
(require 'webdev-keybinds)

(use-package web-mode
  :config
  (setq web-mode-enable-auto-closing t
	web-mode-enable-auto-opening t
	web-mode-enable-auto-indentation t
	web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

;; setup file extensions for web mode
(setq auto-mode-alist (append auto-mode-alist '(
						("\\.html.eex\\'" . web-mode)
						("\\.jsx?\\'" . web-mode))))
;;(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

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
