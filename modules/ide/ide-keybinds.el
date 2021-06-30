;;; ide-keybinds.el -*- lexical-binding:t -*-

;;; Code:
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

;; company keybinds
(general-imap
  :keymaps 'company-mode-map
  "C-j" 'company-select-next
  "C-k" 'company-select-previous)

(open-leader-def
 :keymaps 'normal
 "g" 'magit-status)

;; toggle keybinds
(general-create-definer toggle-leader-def
  :prefix "SPC t")

(toggle-leader-def
  :states '(normal visual)
  "N" 'my-relative-linum-toggle)

;; lsp keybinds
(general-nmap
  :keymaps 'lsp-mode-map
  "M-b" 'lsp-find-definition
  "M-D" 'dap-hydra
  "SPC R" 'lsp-rename)

(general-nmap
  :keymaps 'lsp-ui-peek-mode-map
  "C-k" 'lsp-ui-peek--select-prev
  "C-j" 'lsp-ui-peek--select-next
  "C-h" 'lsp-ui-peek--select-prev-file
  "C-l" 'lsp-ui-peek--select-next-file)

(provide 'ide-keybinds)
;;; ide-keybinds.el ends here
