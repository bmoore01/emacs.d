;; keybinds.el --- All keybinds for my config -*- lexical-binding:t -*-

;;; Commentary:
;;; All of my keybinds

;;; Code:

;; ----------------------------
;; Variables
;; ----------------------------

;; switch mac keybinds
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)


;; ----------------------------
;; Utility functions
;; ----------------------------
(require 'cl-lib)

(cl-defmacro create-leader-key-prefix (name &rest params &key which-key &allow-other-keys)
  "Just a wrapper for `general-create-definer' for simpler which-key naming.
`NAME' is the name of the definer and `PARAMS' is everything else and
`WHICH-KEY' is the name of the prefix in which-key buffer"
  (declare (indent defun))
  (let ((other-keys (cl-loop for (k v) on params by 'cddr
			      unless (eq k :which-key)
			      collect k
			      and collect v)))
    `(general-create-definer
      ,name
      ,@other-keys
      "" (quote (:ignore t :which-key ,which-key)))))

(defun bind-for-projectile (projectile-function regular-function)
  "Bind to `projectile-function' if current dir is projectile project and `regular-function' otherwise."
  (interactive)
  (if (projectile-project-p)
      (call-interactively projectile-function)
    (call-interactively regular-function)))

;; ----------------------------
;; Keybind definitions
;; ----------------------------

(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
 "b" '(lambda () (interactive) (bind-for-projectile 'helm-projectile-switch-to-buffer 'helm-buffers-list))
 "f" 'helm-do-ag-buffers
 "B" 'helm-buffers-list
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
  "f" 'helm-find-files)

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
 "s" 'neotree-hidden-file-toggle)

;; helm keys
(general-define-key
 :keymaps '(helm-map)
 "C-j" 'helm-next-line
 "C-k" 'helm-previous-line
 "C-p" 'helm-execute-persistent-action
 "C-h" 'helm-previous-source
 "C-l" 'helm-next-source)

(general-define-key
 :keymaps '(helm-buffer-map)
 "C-d" 'helm-buffer-run-kill-buffers)

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
  "SPC" 'helm-M-x)

(print (read-directory-name (format "Search in (%s): " default-directory)))

;; TODO add directory comletion look in helm-files.el
;;;###autoload
(defun helm-grep-ag-with-dir-prompt (arg)
  "Same a `helm-do-grep-ag' but it prompts user for search directory first"
  (interactive "P")
  (require 'helm-files)

  (let ((search-dir (expand-file-name (read-directory-name (format "Search in (%s): " default-directory) nil nil t))))
    (helm-grep-ag search-dir arg)))

(general-define-key
 :states '(normal visual)
 "TAB" 'indent-region
 "M-f" 'helm-do-ag-this-file
 "M-F" 'helm-grep-ag-with-dir-prompt
 "C-a" 'evil-numbers/inc-at-pt
 "C-x" 'evil-numbers/dec-at-pt
 "M-]" 'next-buffer :which-key "next buffer"
 "M-[" 'previous-buffer :which-key "previous buffer"
 "M-b" 'xref-find-references
 "M-B" 'xref-find-definitions

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
  "f" 'helm-projectile-find-file
  "s" 'helm-projectile-switch-project
  "o" 'projectile-switch-open-project
  "c" 'projectile-compile-project
  "b" 'projectile-display-buffer
  "w" 'projectile-save-project-buffers
  "/" 'helm-do-ag-project-root)

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
 "TAB" 'lisp-indent-line)
 ;; "M-b" 'find-function-at-point)

(help-leader-def
 :states '(normal visual)
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "f" 'helm-apropos)

;; company keybinds
(general-imap
  :keymaps 'company-mode-map
  "C-j" 'company-select-next
  "C-k" 'company-select-previous)


;; lsp keybinds
(general-nmap
  :keymaps 'lsp-mode-map
  "M-r" 'lsp-find-references
  "M-D" 'dap-hydra
  "SPC R" 'lsp-rename
  "M-RET" 'dap-eval)

(provide 'keybinds)
;;; keybinds.el ends here
