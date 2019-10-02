;; emacs-lisp-keybinds.el
(provide 'emacs-lisp-keybinds)

(major-mode-leader-def
 :states '(normal visual)
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "e" 'eval-last-sexp
 "b" 'eval-buffer
 "r" 'eval-reigon
 "p" 'eval-print-last-sexp)

(general-nmap
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "TAB" 'lisp-indent-line)

(help-leader-def
 :states '(normal visual)
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "f" 'describe-function
 "v" 'describe-variable)
