;;; emacs-lisp-keybinds.el --- Summary
;;; Commentary:
;;; keybinds for elisp trying to keep as close to LSP keybinds as possible
;;; this way can have similar keybinds for all languages;
;;; Code:

(major-mode-leader-def
 :states '(normal visual)
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "e" 'eval-last-sexp
 "b" 'eval-buffer
 "r" 'eval-reigon
 "p" 'eval-print-last-sexp)

(general-nmap
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "TAB" 'lisp-indent-line
 "M-b" 'find-function-at-point)

(help-leader-def
 :states '(normal visual)
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "f" 'counsel-describe-function
 "k" 'counsel-descbinds
 "v" 'counsel-describe-variable)

(provide 'emacs-lisp-keybinds)
;;; emacs-lisp-keybinds.el ends here
