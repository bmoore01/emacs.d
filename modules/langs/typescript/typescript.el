;;; typescript.el --- Summary -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:
(require 'typescript-funcs)
(require 'typescript-keybinds)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(provide 'typescript)
;;; typescript.el ends here
