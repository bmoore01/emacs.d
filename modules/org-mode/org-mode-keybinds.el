;;; org-mode-keybinds.el --- Summary
;;; Commentary:
;;; Code:

(general-create-definer agenda-leader-def
  :prefix "SPC a")

(general-define-key
 :keymaps 'evil-org-mode-map
 :major-modes 'org-mode
 :states '(normal visual)
 "TAB" 'org-cycle
 "RET" 'org-open-at-point
 "-" 'org-cycle-list-bullet
 "t" 'org-todo)

(agenda-leader-def
 :states '(normal visual)
 "a" 'org-agenda-list)

(provide 'org-mode-keybinds)
;;; org-mode-keybinds.el ends here
