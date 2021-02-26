;;; C-C++.el --- All things for writing and debugging C and C++
;;; Commentary:
;;; Code:
(require 'C-C++-funcs)
(require 'C-C++-keybinds)

;;(use-package ccls
;;  :ensure t
;;  :config
;;  (setq ccls-executable "ccls")
;;  (setq lsp-prefer-flymake nil)
;;  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;  :hook ((c-mode c++-mode objc-mode) .
;;         (lambda () (require 'ccls) (lsp))))

(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-hook #'setup-dap-nodes)
(add-hook 'c++-mode-hook #'setup-dap-nodes)

(add-to-list 'projectile-globally-ignored-directories ".ccls-cache")

;;; Insert code here

(provide 'C-C++)
;;; C-C++.el ends here
