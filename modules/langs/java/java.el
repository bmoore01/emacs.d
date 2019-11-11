;;; java.el --- Summary
;;; Commentary:
;;; Code:
(require 'java-funcs)
(require 'java-keybinds)

(use-package lsp-java
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(require 'dap-java)

(provide 'java)
;;; java.el ends here
