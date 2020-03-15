;;; core.el --- Summary
;;; Commentary:
;;; Code:
(require 'core-funcs)

;; make sure this is where you cloned your config otherwise bad news
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

;; disable messages buffer (comment these lines out for debugging)
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Disabled *Completions* buffer
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))

(use-package evil
  :init
  (evil-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers)

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
  :custom
  (neo-hide-cursor t)
  (neo-auto-indent-point t)
  (neo-create-file-auto-open t)
  (neo-smart-open t)
  :commands neo-global--window-exists-p)

(use-package ivy
  :config

  (ivy-mode 1)
  (setq ivy-display-style nil
	ivy-use-virtual-buffers t
	ivy-initial-inputs-alist nil
	ivy-count-format "[%d/%d]"))

;; put back after black screen bug is fixed with mac maybe
;;(use-package ivy-posframe
;;  :after ivy
;;  :diminish
;;  :config
;;  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
;;        ivy-posframe-height-alist '((t . 20)))
;;  (if (member "Source Code Variable" (font-family-list))
;;      (setq ivy-posframe-parameters '((internal-border-width . 10) (font . "Source Code Variable-14:weight=regular")))
;;    ivy-posframe-parameters '((internal-border-width . 10)))
;;  (setq ivy-posframe-width 70)
;;  (ivy-posframe-mode +1))

(use-package ivy-rich
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (all-the-icons-icon-for-mode major-mode)))
  :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel)
(use-package swiper)

(use-package shell-pop
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  (custom-set-variables
   '(shell-pop-full-span t)
   '(shell-pop-window-size 30)))

;;(use-package persp-mode
;;  :hook (after-init . (lambda () (persp-mode 1))))

(require 'core-keybinds)
(add-all-modules)
(add-langs)
(provide 'core)
;;; core.el ends here
