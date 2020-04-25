;;; clojure-keybinds.el --- Summary
;;; Commentary:
;;; Code:

(major-mode-leader-def
 :states '(normal visual)
 :keymaps '(cider-mode-map clojure-mode-map)
 "e" 'cider-eval-last-sexp
 "b" 'cider-load-buffer
 "r" 'cider-eval-reigon
 "p" 'cider-eval-print-last-sexp)

(general-define-key
 :states '(normal visual)
 :keymaps '(cider-stacktrace-mode-map)
    "C-k" 'cider-stacktrace-previous-cause
    "C-j" 'cider-stacktrace-next-cause
    "M-." 'cider-stacktrace-jump
    "q" 'cider-popup-buffer-quit-function
    "J" 'cider-stacktrace-toggle-java
    "c" 'cider-stacktrace-toggle-clj
    "r" 'cider-stacktrace-toggle-repl
    "t" 'cider-stacktrace-toggle-tooling
    "d" 'cider-stacktrace-toggle-duplicates
    "p" 'cider-stacktrace-show-only-project
    "a" 'cider-stacktrace-toggle-all
    "1" 'cider-stacktrace-cycle-cause-1
    "2" 'cider-stacktrace-cycle-cause-2
    "3" 'cider-stacktrace-cycle-cause-3
    "4" 'cider-stacktrace-cycle-cause-4
    "5" 'cider-stacktrace-cycle-cause-5
    "0" 'cider-stacktrace-cycle-all-causes
    "TAB" 'cider-stacktrace-cycle-current-cause
    "backtab" 'cider-stacktrace-cycle-all-causes)


(provide 'clojure-keybinds)
;;; clojure-keybinds.el ends here
