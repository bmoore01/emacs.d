;;; ui-keybinds.el --- Summary -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(general-create-definer theme-leader-def
  :prefix "SPC T")

(theme-leader-def
  :keymaps '(normal visual)
  "n" 'ui-cycle-themes
  "m" 'thin-modeline-mode)

(general-define-key
 :states '(normal visual)
 "M-=" '(lambda () (interactive) (global-text-scale-adjust 1))
 "M--" '(lambda () (interactive) (global-text-scale-adjust -1)))


(provide 'ui-keybinds)
;;; ui-keybinds.el ends here
