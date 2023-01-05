;;; core.el --- The loader of modules -*- lexical-binding:t -*-
;;; Commentary:
;;; Might factor window popup into a seperate package for use at some point but until then it's going to live in here.
;;; Code:
(require 'core-lib)
(require 'core-funcs)

;; this has to be called first before anything
;; as use-package is used pretty much everywhere
(setup-use-package)

;; set global constants
(defconst modules-dir (concat user-emacs-directory "modules/"))
(defconst langs-dir (concat modules-dir "langs/"))
(defconst user-secrets-dir (concat user-emacs-directory "private/")
  "Secrets directory for all things that shouldn't be uploaded to git, passwords, privte keys etc.")

;; set defaults
(setq-default
 cursor-type 'bar
 explicit-shell-file-name "/bin/zsh"
 compilation-always-kill t
 compilation-scroll-output t
 show-paren-delay 0)

;; set ui defaults
;; (tool-bar-mode -1)
(scroll-bar-mode -1)

;; split windows for popups vertically
(setq split-width-threshold nil)

;; set shell env for eshell
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

;; set exec path for projectile and stuff
(add-path-string-to-exec-path
 "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:usr/local/opt/llvm/bin:/usr/bin")

;; replace all yes or no prompts with y/n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; avoid making a mess in the filesystem
(setq create-lockfiles nil
      make-backup-files nil
      backup-by-copying t
      backup-directory-alist '(("." . (concat user-emacs-directory "backups/")))
      delete-old-versions t
      ring-bell-function 'ignore
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

;; set tab width
(setq tab-width 4)
(setq evil-shift-width tab-width)

(defvar scratch-mode 'lisp-interaction-mode
  "Default major mode of the scratch buffer.")

;; highlight parens
(show-paren-mode 1)

;; show line numbers only in code buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; set theme
;; (load-theme 'shades-of-purple t nil)

;; disable messages buffer
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Disable *Completions* buffer
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))

(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-respect-visual-line-mode t)
  :hook
  ((evil-mode . (lambda ()
		  (dolist (mode '(custom-mode
				  eshell-mode
				  git-rebase-mode
				  erc-mode
				  term-mode))
		  (add-to-list 'evil-emacs-state-modes mode)))))
  :config
  (evil-mode t)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Set initial state for some buffers that are insert by default
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :diminish
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :diminish)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package general
  :after evil
  :config (general-evil-setup t))

(use-package which-key
  :diminish
  :init
  (which-key-mode))

(use-package hydra
  :defer 1)

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
  (setq neo-window-width 32)
  (setq neo-theme (if (display-graphic-p) 'icons))
  :custom
  (neo-hide-cursor t)
  (neo-auto-indent-point t)
  (neo-create-file-auto-open t)
  (neo-smart-open t)
  :commands neo-global--window-exists-p)

;; darker theme if needed
(use-package shades-of-purple-theme)

(use-package all-the-icons)
(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package git-gutter
  :ensure git-gutter-fringe
  :hook (prog-mode . git-gutter-mode))

(use-package ivy
  :diminish
  :init
  (ivy-mode 1)
  :config
  (setq ivy-wrap t
	;;ivy-display-style nil
	ivy-use-virtual-buffers t
	ivy-initial-inputs-alist nil
	ivy-count-format "(%d/%d) "))

(use-package ivy-hydra
  :defer t
  :after hydra)

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
            (ivy-rich-candidate)
            (ivy-rich-switch-buffer-project (:face success))
            (ivy-rich-switch-buffer-major-mode (:face warning)))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:face font-lock-doc-face))))))
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode t))

(use-package counsel
  :config (counsel-mode t))

(use-package swiper)

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :config (projectile-mode)
  :init
  (progn
    (when (file-directory-p "~/workspace")
      (setq projectile-project-search-path '("~/workspace")))
    (setq projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class")
	projectile-ignored-projects '("~/" "/tmp"))))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package linum-relative
  :config
  (setq linum-relative-backend 'display-line-numbers-mode))

(use-package company
  :init
  (setq company-idle-delay 0.0
        company-global-modes '(not org-mode)
        company-minimum-prefix-length 1)

  (global-company-mode 1)
  (add-to-list 'company-backends 'company-files))

;; (use-package company-box
  ;; :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :commands lsp
  :hook
  (sh-mode . lsp)
  :config
  (setq lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-enable-file-watchers nil
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-highlight-symbol-at-point nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil)

  (setq lsp-idle-delay 0.500)

  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)

  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

(use-package lsp-ui
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-delay 0.0
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (setq dap-ui-locals-expand-depth 3)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (require 'dap-gdb-lldb))

(use-package yasnippet
  :config (yas-global-mode))

(use-package magit)

(use-package python-mode
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  :config
  (require 'dap-python))

(require 'core-keybinds)
(provide 'core)
;;; core.el ends here
