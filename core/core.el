;; core.el
(require 'core-funcs)

(defconst modules-dir (concat user-emacs-directory "modules/"))
(defconst langs-dir (concat modules-dir "langs/"))

;; this has to be called first before anything
(initialise-core)

;; set defaults
(setq-default
 cursor-type 'bar
 explicit-shell-file-name "/bin/zsh"
 eshell-path-env (concat "~/.zshrc")
 create-lockfiles nil
 make-backup-files nil
 compilation-always-kill t
 compilation-scroll-output t
 show-paren-delay 0
 ;; secrets dir for passwords etc
 user-secrets-dir "~/.emacs.d/private/"
 evil-want-C-u-scroll t)

;; set shell env for eshell
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

;; set exec path for projectile and stuff
(add-path-string-to-exec-path
 "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:usr/local/opt/llvm/bin:/usr/bin")

(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'exec-path "/usr/local/bin")

;; avoid making a mess in the filesystem
(setq backup-by-copying t
      backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
      delete-old-versions t
      ring-bell-function 'ignore
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      use-package-always-ensure t)
      ;;gdb-many-windows t
      ;;gdb-show-main t)

(setq use-package-always-ensure t)

(defvar scratch-mode 'lisp-interaction-mode
  "Default major mode of the scratch buffer.")

;; highlight parens
(show-paren-mode 1)

;; show line numbers only in code buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
	    (setq eshell-command-aliases-list my-eshell-aliases)))

(use-package evil
  :init
  (evil-mode))

(use-package general
  :after evil
  :config (general-evil-setup))

(use-package which-key
  :init
  (which-key-mode))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(use-package neotree
  :config
  (add-hook 'neo-after-create-hook (lambda (&rest _)
				     (setq
				      mode-line-format nil
				      left-margin-width 2)
				     (set-window-buffer nil (current-buffer))))
  :commands neo-global--window-exists-p)

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "[%d/%d] "))

(use-package counsel)
(use-package swiper)

;;(use-package projectile
;;  :config
;;  (projectile-mode)
;;  (setq projectile-project-search-path '("~/workspace")
;;	projectile-completion-system 'ivy))
;;
;;(use-package ag)
;;(use-package wgrep)
;;(use-package counsel-projectile
;;  :after projectile
;;  :config
;;  (counsel-projectile-mode))

(require 'core-keybinds)
(add-all-modules)
(add-langs)
(provide 'core)
;;; core.el ends here
