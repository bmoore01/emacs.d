;;; python-lang-keybinds.el --- Summary
;;; Commentary:
;;; Code:

(major-mode-leader-def
 :states '(normal visual)
 :keymaps '(python-mode-map)
  "b" '(lambda () (interactive) (check-python-running python-shell-send-buffer)))
  ;;; TODO fix this  I need to pass the region
  ;;;"r" '(lambda () (interactive) (check-python-running python-shell-send-region)))

(open-leader-def
 :keymaps 'normal
 "p" 'python-popup-toggle)

(provide 'python-lang-keybinds)
;;; python-lang-keybinds.el ends here
