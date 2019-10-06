;; ide --- Summary
;; This module provides all the ide-like functionality in my emacs config
;; lsp for language server interactions
;; projectile for project management
;; flycheck for syntax checking

;;; Commentary:
;; lsp documentation stuff needs work to figure out what goes best
;; also add and make use of Emacs new tabs feature at some point

;;; Code:
(require 'ide-funcs)

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-project-search-path '("~/workspace")
	projectile-completion-system 'ivy))

(use-package ag)
(use-package wgrep)
(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package company
  :hook (prog-mode . global-company-mode))

(use-package linum-relative
  :config
  (setq linum-relative-backend 'display-line-numbers-mode))

;;(use-package lsp-mode
;;  :commands lsp
;;  ;;:custom
;;  ;;(lsp-print-io nil)
;;  ;;(lsp-trace nil)
;;  ;;(lsp-print-performance nil)
;;  ;;(lsp-document-sync-method 'incremental)
;;  ;;(lsp-response-timeout 10)
;;  ;;(lsp-prefer-flymake t)
;;  :config
;;  (use-package company-lsp
;;    :config
;;    (add-to-list 'company-backends 'company-lsp))
;;  (setq lsp-enable-snippet nil))

;;(use-package lsp-ui
  ;;:after '(lsp flycheck)
  ;;:hook (lsp-mode . lsp-ui-mode))
  ;;:config
  ;;(setq lsp-ui-doc-enable nil
	;;lsp-ui-imenu-enable t
	;;lsp-ui-flycheck-enable t
	;;lsp-ui-sideline-enable t
	;;lsp-ui-sideline-ignore-duplicate t))

(require 'ide-keybinds)
(provide 'ide)
;;; ide.el ends here
