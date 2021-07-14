;;; org-mode-keybinds.el --- Keybinds for org-mode
;;; Commentary:
;;; Code:

(general-define-key
 :major-modes 'org-mode
 :states '(normal visual)
 "C-c C-t" 'org-todo
 "C-c C-w" 'org-refile)

(general-define-key
 :major-modes 'org-mode
 :states '(normal visual)
 "TAB" 'org-cycle
 "RET" 'org-open-at-point
 "-" 'org-cycle-list-bullet)

(open-leader-def
 :states '(normal visual)
 "a" 'org-agenda
 "c" 'org-capture)

(major-mode-leader-def
 :states '(normal visual)
 :keymaps '(evil-org-mode-map org-mode-map)
 "," 'org-insert-structure-template)

(provide 'org-mode-keybinds)
;;; org-mode-keybinds.el ends here
