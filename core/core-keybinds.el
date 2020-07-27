;; core-keybinds.el --- Keybinds i can't live without

;;; Commentary:
;;; Could potentially extract out ivy stuff and even window stuff

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
 "b" '(lambda () (interactive) (if (projectile-project-p)
				   (call-interactively 'counsel-projectile-switch-to-buffer)
				 (call-interactively 'ivy-switch-buffer)))
 "B" 'ivy-switch-buffer
 ;; home
 "s" 'switch-to-scratch-buffer
 "N" 'new-empty-buffer
 "d" 'kill-current-buffer
 "n" 'next-buffer
 "p" 'previous-buffer
 "]" 'next-buffer
 "[" 'previous-buffer)

;; open things bindings
(general-create-definer open-leader-def
  :prefix "SPC o")

(open-leader-def
 :keymaps 'normal
 "t" 'neotree-toggle)

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
 "C-u" 'ivy-scroll-up-command
 "TAB" 'ivy-partial-or-done)

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
 "/" 'counsel-grep-or-swiper
 "?" 'counsel-grep-or-swiper-backward
 "C-a" 'evil-numbers/inc-at-pt
 "C-x" 'evil-numbers/dec-at-pt
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
 "M-W" 'eyebrowse-close-window-config)

(provide 'core-keybinds)
;;; core-keybinds.el ends here
