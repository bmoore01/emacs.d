;;; ui-keybinds.el --- Summary
;;; Commentary:
;;; Code:

(general-create-definer theme-leader-def
  :prefix "SPC T")

(theme-leader-def
  :keymaps '(normal visual)
  "n" 'ui-cycle-themes)

(provide 'ui-keybinds)
;;; ui-keybinds.el ends here
