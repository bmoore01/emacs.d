;;; rust-keybinds.el --- Summary
;;; Commentary:
;;; Code:

(major-mode-leader-def
  :states '(normal)
  :keymaps 'rust-mode-map
  "b" 'rust-run)
;; possible additions
;;   (("t" rust-backtrace "backtrace"))
;;   (("d" racer-find-definition "find definition"))))

(provide 'rust-keybinds)
;;; rust-keybinds.el ends here
