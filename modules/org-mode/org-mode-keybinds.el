;;; org-mode-keybinds.el --- Keybinds for org-mode
;;; Commentary:
;;; Code:

(general-define-key
 :prefix "C-c")

(general-define-key
 :keymaps 'evil-org-mode-map
 :major-modes 'org-mode
 :states '(normal visual)
 "c" 'org-capture
 "C-t" 'org-todo)

(general-define-key
 :keymaps 'evil-org-mode-map
 :major-modes 'org-mode
 :states '(normal visual)
 "TAB" 'org-cycle
 "RET" 'org-open-at-point
 "-" 'org-cycle-list-bullet)

(open-leader-def
 :states '(normal visual)
 "a" 'org-agenda-list)

(major-mode-leader-def
 :states '(normal visual)
 :keymaps '(evil-org-mode-map org-mode-map)
 "," 'org-insert-structure-template)

(provide 'org-mode-keybinds)
;;; org-mode-keybinds.el ends here
