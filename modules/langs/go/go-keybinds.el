;;; go-keybinds.el --- Summary
;;; Commentary:
;;; Code:


;;; Insert code here
 (setq compilation-read-command nil)
  ;;  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (local-set-key (kbd "M-,") 'compile)

(major-mode-leader-def
 :states '(normal visual)
 :keymaps 'go-mode-map
 "b" 'compile)

(provide 'go-keybinds)
;;; go-keybinds.el ends here
