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
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

(use-package company
  :hook (prog-mode . global-company-mode))

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
  :commands lsp
  :hook (prog-mode . lsp)
  :custom
  (lsp-auto-guess-root nil)
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  :config
  (setq lsp-keep-workspace-alive nil
	lsp-auto-guess-root t)
  (use-package company-lsp
    :config
    (add-to-list 'company-backends 'company-lsp)
    :commands company-lsp
    :custom
    (company-lsp--cache-item-candidates t)
    (company-lsp--candidates-async t))
  (setq lsp-enable-snippet t))
  ;;(setq lsp-enable-snippet nil))
  ;;(add-to-list 'exec-path "/usr/local/opt/llvm/bin/clangd"))

(use-package lsp-ui
  :requires lsp-mode flycheck
  :hook (lsp-mode . lsp-ui-mode)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :config
  (setq lsp-ui-doc-use-childframe t
	lsp-ui-doc-max-width 120
	lsp-ui-doc-max-height 30
	;;lsp-ui-doc-use-webkit nil ;; can't compile with xwidgets
	lsp-ui-flycheck-enable t
	lsp-ui-flycheck-list-position 'right
	lsp-ui-flycheck-live-reporting t
	lsp-ui-peek-enable t
	lsp-ui-peek-list-width 60
	lsp-ui-peek-list-height 25
	lsp-ui-sideline-ignore-duplicate t)
  (custom-set-variables
   lsp-ui-doc-header t
   lsp-ui-doc-include-signature t
   lsp-ui-doc-border (face-foreground 'default)
   lsp-ui-sideline-enable nil
   lsp-ui-ignore-duplicate t
   lsp-ui-sideline-show-code-actions nil
   lsp-ui-doc-position 'top
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-doc-enable nil))

(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup))

;; TODO: add some of these sweet snippets
(use-package yasnippet)

(use-package magit)
(use-package evil-magit)

(require 'ide-keybinds)
(provide 'ide)
;;; ide.el ends here
