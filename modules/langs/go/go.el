;;; go.el --- Summary
;;; Commentary:
;;; Code:
(require 'go-funcs)
(require 'go-keybinds)

(use-package go-mode
  :mode (("\\.go$" . go-mode)))

;; dap setup
(require 'dap-go)
(dap-go-setup)

(provide 'go)
;;; go.el ends here
