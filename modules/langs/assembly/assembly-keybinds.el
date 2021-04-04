;;; assembly-keybinds.el --- Summary -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:


(general-nmap
 :keymaps '(nasm-mode-map)
 "TAB" 'lisp-indent-line
 "M-b" 'find-function-at-point)

(provide 'assembly-keybinds)
;;; assembly-keybinds.el ends here
