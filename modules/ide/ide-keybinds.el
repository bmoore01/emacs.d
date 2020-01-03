;; ide-keybinds.el --- Summary

;;;Code:
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

;; toggle keybinds
(general-create-definer toggle-leader-def
  :prefix "SPC t")

(toggle-leader-def
  :states '(normal visual)
  "N" 'my-relative-linum-toggle)

;; lsp keybinds
(general-nmap
  :keymaps 'lsp-mode-map
  "SPC R" 'lsp-rename)

(general-create-definer lsp-leader-def
  :prefix "SPC l")

(lsp-leader-def
 :keymaps 'lsp-mode-map
 :states '(normal visual)
 "r" 'lsp-ui-peek-find-references
 "j" 'lsp-ui-peek-find-definitions
 "i" 'lsp-ui-peek-find-implementation
 "m" 'lsp-ui-imenu
 "s" 'lsp-ui-sideline-mode
 "d" 'my-toggle-lsp-ui-doc
 "D" 'dap-debug)

(lsp-leader-def
  :keymap 'dap-mode-map
  :states '(normal visual)
  "s" 'dap-step-in
  "S" 'dap-step-out
  "c" 'dap-continue
  "b" 'dap-breakpoint-toggle)

(general-nmap
  :keymaps 'lsp-ui-imenu-mode-map
  "q" 'lsp-ui-imenu--kill
  ;;"<right>" 'lsp-ui-imenu--next-kind
  ;;"<left>" 'lsp-ui-imenu--prev-kind
  ;;"<return>" 'lsp-ui-imenu--view
  ;;"<M-return>" 'lsp-ui-imenu--visit
  "M-RET" 'lsp-ui-imenu--view
  "RET" 'lsp-ui-imenu--visit)

(general-nmap
  :keymaps 'lsp-ui-peek-mode-map
  "C-k" 'lsp-ui-peek--select-prev
  "C-j" 'lsp-ui-peek--select-next
  "C-h" 'lsp-ui-peek--select-prev-file
  "C-l" 'lsp-ui-peek--select-next-file)

(general-nmap
  :keymaps 'lsp-mode-map
  "M-s-b" 'lsp-find-implementation
  "M-b" 'lsp-find-definition)

;;    (define-key map "\e\e\e" 'lsp-ui-peek--abort)
;;    (define-key map "\C-g" 'lsp-ui-peek--abort)
;;    (define-key map (kbd "M-n") 'lsp-ui-peek--select-next-file)
;;    (define-key map (kbd "<right>") 'lsp-ui-peek--select-next-file)
;;    (define-key map (kbd "M-p") 'lsp-ui-peek--select-prev-file)
;;    (define-key map (kbd "<left>") 'lsp-ui-peek--select-prev-file)
;;    (define-key map (kbd "C-n") 'lsp-ui-peek--select-next)
;;    (define-key map (kbd "n") 'lsp-ui-peek--select-next)
;;    (define-key map (kbd "<down>") 'lsp-ui-peek--select-next)
;;    (define-key map (kbd "C-p") 'lsp-ui-peek--select-prev)
;;    (define-key map (kbd "p") 'lsp-ui-peek--select-prev)
;;    (define-key map (kbd "<up>") 'lsp-ui-peek--select-prev)
;;    (define-key map (kbd "TAB") 'lsp-ui-peek--toggle-file)
;;    (define-key map (kbd "q") 'lsp-ui-peek--abort)
;;    (define-key map (kbd "RET") 'lsp-ui-peek--goto-xref)
;;    (define-key map (kbd "M-RET") 'lsp-ui-peek--goto-xref-other-window)

(provide 'ide-keybinds)
;;; ide-keybinds.el ends here
