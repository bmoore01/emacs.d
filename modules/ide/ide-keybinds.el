;; ide-keybinds.el

;; projectile key bindings
(general-create-definer projectile-leader-def
  :prefix "SPC p")

(projectile-leader-def
  :keymaps 'normal
  "f" 'projectile--find-file
  "s" 'projectile-switch-project
  "o" 'projectile-switch-open-project
  "c" 'projectile-compile-project
  "b" 'projectile-display-buffer
  "w" 'projectile-save-project-buffers
  "/" 'projectile-ag)

(general-imap
  :keymaps 'company-mode-map
  "C-j" 'company-select-next
  "C-k" 'company-select-previous)

(provide 'ide-keybinds)
;;; ide-keybinds.el ends here
