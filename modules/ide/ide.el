;; ide --- all things trying to replicate IDE features -*- lexical-binding:t -*-
;; This module provides all the ide-like functionality in my emacs config
;; lsp for language server interactions
;; projectile for project management
;; flycheck for syntax checking

;;; Commentary:
;; lsp documentation stuff needs work to figure out what goes best
;; also add and make use of Emacs new tabs feature at some point

;;; Code:
(require 'ide-funcs)

(defvar gdb-many-windows t)
(defvar gdb-show-main t)

(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :config (projectile-mode)
  :init
  (setq projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class")
	projectile-ignored-projects '("~/" "/tmp")))

(use-package ag)
(use-package wgrep)
(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

(use-package linum-relative
  :config
  (setq linum-relative-backend 'display-line-numbers-mode))

(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  ;;(sp-pair "=" "=" :actions '(wrap))
  ;;(sp-pair "+" "+" :actions '(wrap))
  ;;(sp-pair "$" "$" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (
	 (lsp-mode . lsp-enable-which-key-integration)
	 (prog-mode . lsp-deferred)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
 :hook (company-mode . company-box-mode))

(use-package dap-mode
  :config
  (require 'dap-node)
  (dap-node-setup)
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup))

;; TODO: add some snippets
(use-package yasnippet)

(use-package magit)
(use-package evil-magit
  :after magit)

(require 'ide-keybinds)
(provide 'ide)
;;; ide.el ends here
