;;; C-C++.el --- All things for writing and debugging C and C++
;;; Commentary:
;;; Code:
(require 'C-C++-funcs)
(require 'C-C++-keybinds)

(setq c-default-style "linux"
      c-basic-offset 4)

(use-package ccls
  :config
  (setq lsp-prefer-flymake nil)
  :hook ((c-mode c++-mode objc-mode cuda-mode) .(lambda () (require 'ccls) (lsp))))

(add-hook 'c-mode-hook #'setup-dap-nodes)
(add-hook 'c++-mode-hook #'setup-dap-nodes)

(add-to-list 'projectile-globally-ignored-directories ".ccls-cache")

;;; Insert code here

(provide 'C-C++)
;;; C-C++.el ends here
