;; ui-keybinds.el
(provide 'ui-keybinds)

(general-create-definer theme-leader-def
  :prefix "SPC T")

(theme-leader-def
  :keymaps '(normal visual)
  "n" 'ui-cycle-themes)
