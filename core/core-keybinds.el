;; core-keybinds.el --- Summary

;;; Commentary:
;;; All the keybindings that are essential to how I use Emacs.

;;; Code:

;; switch mac keybinds
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; window keybinds
(general-create-definer window-leader-def
  :prefix "SPC w"
  :which-key "window")

(window-leader-def
 :keymaps 'normal
 "v" 'split-window-and-follow-horizontally
 "s" 'split-window-and-follow-vertically
 "d" 'delete-window
 "j" 'evil-window-down
 "k" 'evil-window-up
 "l" 'evil-window-right
 "h" 'evil-window-left)

;; buffer keybinds
(general-create-definer buffer-leader-def
  :prefix "SPC b"
  :which-key "buffer")

(general-define-key
 :states '(normal visual)
  "M-]" 'next-buffer :which-key "next buffer"
  "M-[" 'previous-buffer :which-key "previous buffer")

(buffer-leader-def
 :keymaps 'normal
 "b" 'ivy-switch-buffer
 ;; home
 "s" 'switch-to-scratch-buffer
 "N" 'new-empty-buffer
 "d" 'kill-current-buffer
 "n" 'next-buffer
 "p" 'previous-buffer
 "]" 'next-buffer
 "[" 'previous-buffer)

;; searching
(general-nmap
  "M-f" 'occur)

;; open things bindings
(general-create-definer open-leader-def
  :prefix "SPC o")

(open-leader-def
 :keymaps 'normal
 "t" 'neotree-toggle
 "e" 'eshell-toggle
 "i" 'elisp-repl-toggle
 "s" 'shell-pop)
 ;;"s" 'shell-toggle) ;; rip my shell toggle, hope I can find a fix for this someday D:

;; file key bindings
(general-create-definer file-leader-def
  :prefix "SPC f")

 ;; TODO: add open emacs config as projectile project
(file-leader-def
  :keymaps 'normal
  "f" 'counsel-find-file)

(general-create-definer help-leader-def
  :prefix "SPC h")

(general-create-definer major-mode-leader-def
  :prefix "SPC m")

;; neotree keys
(general-define-key
 :keymaps 'neotree-mode-map
 :states '(normal visual insert emacs)
 "<escape>" 'neotree-hide
 "q" 'neotree-hide
 "RET" 'neotree-enter
 "TAB" 'neotree-stretch-toggle
 "|" 'neotree-enter-vertical-split
 "-" 'neotree-enter-horizontal-split
 "'" 'neotree-quick-look
 "K" 'neotree-select-up-node
 "c" 'neotree-create-node
 "C" 'neotree-copy-node
 "d" 'neotree-delete-node
 "R" 'neotree-change-root
 "r" 'neotree-rename-mode
 "gr" 'neotree-refresh
 "l" 'neotree-expand-or-open
 "h" 'neotree-collapse-or-up
 "M-1" 'neotree-hide
 "s" 'neotree-hidden-file-toggle)

;; ivy keys
(general-define-key
 :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
 "<escape>" 'minibuffer-keyboard-quit
 "C-j" 'ivy-next-line
 "C-k" 'ivy-previous-line
 "C-d" 'ivy-scroll-down-command
 "C-u" 'ivy-scroll-up-command)

(general-define-key
 :keymaps 'ivy-switch-buffer-map
 "C-d" 'ivy-switch-buffer-kill)

(general-nmap
  :prefix "SPC"
  "SPC" 'counsel-M-x)

(general-define-key
 :states '(normal visual)
 "TAB" 'indent-region
 "M-1" 'neotree-show
 "C-a" 'evil-numbers/inc-at-pt
 "C-x" 'evil-numbers/dec-at-pt)

(provide 'core-keybinds)
;;; core-keybinds.el ends here
