;;; terminal.el --- Summary -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:
(require 'terminal-funcs)
(require 'terminal-keybinds)

(use-package shell-pop
  :config
  (setq
   shell-pop-term-shell "/usr/local/bin/zsh"
   shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  :custom
  (shell-pop-full-span t)
  (shell-pop-window-size 30))

(defcustom pop-toggle-size 30
  "Percentage for popup window size."
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (and (integerp x)
                            (<= x 100)
                            (<= 0 x))))))

(defvar my-eshell-aliases
  '(("q"  "exit")           ; built-in
    ("f"  "find-file $1")
    ("bd" "eshell-up $1")
    ("rg" "rg --color=always $*")
    ("ag" "ag --color=always $*")
    ("l"  "ls -lh")
    ("ll" "ls -lah")
    ("clear" "clear-scrollback")
    ("c" "clear-scrollback")))

(add-hook 'eshell-alias-load-hook
	  (lambda ()
	    (defvar eshell-command-aliases-list my-eshell-aliases)))

(provide 'terminal)
;;; terminal.el ends here
