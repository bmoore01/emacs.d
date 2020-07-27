;;; terminal-keybinds.el --- Summary -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(open-leader-def
 :keymaps 'normal
 "e" 'eshell-toggle
 "i" 'elisp-repl-toggle
 "s" 'shell-pop)

(provide 'terminal-keybinds)
;;; terminal-keybinds.el ends here
