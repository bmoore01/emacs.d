;;; assembly.el --- Summary -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:
(require 'assembly-funcs)
(require 'assembly-keybinds)

(use-package x86-lookup
  :ensure t
  :config
  (setq  x86-lookup-pdf "~/Documents/intel-manual/intel-x86-manual.pdf"))
(use-package nasm-mode
  :ensure t
  :config
  (add-hook 'asm-mode-hook 'nasm-mode))


(provide 'assembly)
;;; assembly.el ends here
