;; core-keybinds.el --- Keybinds I can't live without -*- lexical-binding:t -*-

;;; Commentary:
;;; Could potentially extract out ivy stuff and even window stuff

;;; Code:

(require 'core-lib)

;; switch mac keybinds
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; WINDOW keybinds
(create-leader-key-prefix window-leader-def
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
 "h" 'evil-window-left
 "=" 'balance-windows)

;; buffer keybinds
(create-leader-key-prefix buffer-leader-def
  :prefix "SPC b"
  :which-key "buffer")

(buffer-leader-def
 :keymaps 'normal
 "b" '(lambda () (interactive) (if (projectile-project-p)
				   (call-interactively 'counsel-projectile-switch-to-buffer)
				 (call-interactively 'ivy-switch-buffer)))
 "B" 'ivy-switch-buffer
 "s" 'switch-to-scratch-buffer
 "N" 'new-empty-buffer
 "d" 'kill-current-buffer
 "n" 'next-buffer
 "p" 'previous-buffer
 "[" 'evil-jump-backward
 "]" 'evil-jump-forward)

;; open things bindings
(create-leader-key-prefix open-leader-def
  :prefix "SPC o"
  :which-key "open")

(open-leader-def
 :keymaps 'normal
 "g" 'magit-status)

(open-leader-def
 :keymaps 'normal
 "t" 'neotree-toggle)

;; file key bindings
(create-leader-key-prefix file-leader-def
  :prefix "SPC f"
  :which-key "files")

 ;; TODO: add open emacs config as projectile project
(file-leader-def
  :keymaps 'normal
  "f" 'counsel-find-file)

(create-leader-key-prefix help-leader-def
  :prefix "SPC h"
  :which-key "help")

(create-leader-key-prefix major-mode-leader-def
  :prefix "SPC m"
  :which-key "major mode")

;; neotree keys
(general-define-key
 :keymaps 'neotree-mode-map
 :states '(normal visual insert emacs)
 "<escape>" 'neotree-hide
 "q" 'neotree-hide
 "RET" 'neotree-enter
 "TAB" 'neotree-stretch-toggle
 "v" 'neotree-enter-vertical-split
 "s" 'neotree-enter-horizontal-split
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
 "C-j" 'ivy-next-line
 "C-k" 'ivy-previous-line
 "TAB" 'ivy-partial-or-done)

(general-define-key
 :keymaps '(ivy-switch-buffer-map counsel-projectile-switch-to-buffer)
 "C-d" 'ivy-switch-buffer-kill)

(general-nmap
  :prefix "SPC"
  "SPC" 'counsel-M-x)

(general-define-key
 :states '(normal visual)
 "TAB" 'indent-region
 "M-1" 'neotree-show
 "M-f" 'counsel-grep-or-swiper
 "?" 'counsel-grep-or-swiper-backward
 "C-a" 'evil-numbers/inc-at-pt
 "C-x" 'evil-numbers/dec-at-pt
 "M-]" 'next-buffer :which-key "next buffer"
 "M-[" 'previous-buffer :which-key "previous buffer"
 ;; eyebrowse keybinds
 "M-1" 'eyebrowse-switch-to-window-config-1
 "M-2" 'eyebrowse-switch-to-window-config-2
 "M-3" 'eyebrowse-switch-to-window-config-3
 "M-4" 'eyebrowse-switch-to-window-config-4
 "M-5" 'eyebrowse-switch-to-window-config-5
 "M-6" 'eyebrowse-switch-to-window-config-6
 "M-7" 'eyebrowse-switch-to-window-config-7
 "M-8" 'eyebrowse-switch-to-window-config-8
 "M-9" 'eyebrowse-switch-to-window-config-9
 "M-s-<right>" 'eyebrowse-next-window-config
 "M-s-<left>" 'eyebrowse-prev-window-config
 "M-n" 'eyebrowse-create-window-config
 "M-w" 'eyebrowse-close-window-config)


(create-leader-key-prefix projectile-leader-def
  :prefix "SPC p"
  :which-key "project")

(projectile-leader-def
  :keymaps 'normal
  "f" 'projectile--find-file
  "s" 'projectile-switch-project
  "o" 'projectile-switch-open-project
  "c" 'projectile-compile-project
  "b" 'projectile-display-buffer
  "w" 'projectile-save-project-buffers
  "/" 'counsel-projectile-rg)

;; toggle keybinds
(create-leader-key-prefix toggle-leader-def
  :prefix "SPC t"
  :which-key "toggle")

(toggle-leader-def
  :states '(normal visual)
  "n" 'my-relative-linum-toggle)

;; elisp keybinds
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


;; company keybinds
(general-imap
  :keymaps 'company-mode-map
  "C-j" 'company-select-next
  "C-k" 'company-select-previous)


;; lsp keybinds
(general-nmap
  :keymaps 'lsp-mode-map
  "M-b" 'lsp-find-definition
  "M-B" 'lsp-find-implementation
  "M-r" 'lsp-find-references
  "M-D" 'dap-hydra
  "SPC R" 'lsp-rename
  "M-RET" 'dap-eval)

(provide 'core-keybinds)
;;; core-keybinds.el ends here
