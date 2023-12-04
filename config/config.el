;;; core.el --- The loader of modules -*- lexical-binding:t -*-
;;; Commentary:
;;; Might factor window popup into a seperate package for use at some point but until then it's going to live in here.
;;; Code:
(require 'config-funcs)

;; this has to be called first before anything
;; as use-package is used pretty much everywhere
(setup-use-package)

(defconst user-secrets-directory (concat user-emacs-directory "private/")
  "Dir ignored by git for, passwords, private keys etc.")

;; set defaults
(setq-default
 cursor-type 'bar
 compilation-always-kill t
 compilation-scroll-output t
 show-paren-delay 0
 blink-cursor-mode 0)

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

;; set font size 14 by default
(set-face-attribute 'default nil :height 140)

(setq custom-file (concat user-emacs-directory "config/custom.el"))
(when (file-exists-p  custom-file)
  (load custom-file))

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

;; stop xref asking for which identifier to select
(setq xref-prompt-for-identifier nil)

;; show line numbers only in code buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; set theme
;; (load-theme 'shades-of-purple t nil)

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

(use-package helm
  :config
  (helm-autoresize-mode 1))

(use-package helm-projectile
  :after projectile)

(use-package helm-xref)

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
    (when (file-directory-p "~/workspace")
      (setq projectile-project-search-path '("~/workspace"))))

(use-package linum-relative
  :config
  (setq linum-relative-backend 'display-line-numbers-mode))

(use-package yasnippet
  :config (yas-global-mode))

;; (use-package company
;;   :init
;;   (setq company-idle-delay 0.0
;;         company-global-modes '(not org-mode)
;;         company-minimum-prefix-length 1)

;;  (global-company-mode 1)
;;  (add-to-list 'company-backends 'company-files))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :after yasnippet
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
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)) 

  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)

  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

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

;; customise dap mode window placement
;; this doesn't work yet
(defvar dap-ui-buffer-configurations
  `((,dap-ui--locals-buffer . ((side . bottom) (slot . 1) (window-width . 0.45)))
    (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
    (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
    (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
    (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
    (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-height . 0.45)))))

(use-package magit)

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  :config
  (require 'dap-python))


;; (use-package solarized-theme
;;   :config
;;   (load-theme 'solarized-light t))

(use-package minions
  :config (minions-mode)
  :custom (minions-mode-line-lighter "..."))

;; (use-package nano-theme
;;   :ensure nil
;;   :defer t
;;   :quelpa (nano-theme
;;            :fetcher github
;;            :repo "rougier/nano-theme"))

(use-package company
  :ensure t
  :commands (company-mode
             global-company-mode company-complete
             company-complete-common
             company-manual-begin
             company-grab-line)
  :init
  (setq company-idle-delay 0.0
        company-global-modes '(not org-mode)
        company-minimum-prefix-length 1
		company-tooltip-align-annotations t
		company-transformers nil
		company-lsp-async t
		company-lsp-cache-candidates nil)
  (global-company-mode))  

(defface company-icon+ `((t (:inherit company-tooltip)))
  "Face for the margin icon in the `company-mode' tooltip."
  :group 'company-faces)

(defface company-current-icon+ `((t (:inherit company-tooltip-selection)))
  "Face for the margin icon for the current candidate in the `company-mode' tooltip."
  :group 'company-faces)

;;(defconst all-the-icons-lsp-kinds+
;;  (apply
;;   #'cons
;;   (cl-loop
;;    for parent-face in '(company-icon+ company-current-icon+)
;;    ;; TODO: Replace with whatever icons you want to use in place of these LSP kinds.
;;    collect `((text        . ,(all-the-icons-nerd-seti "text"                       :face parent-face))
;;              (method      . ,(propertize "ƒ"                                       'face `(:inherit (an-old-hope-yellow-int ,parent-face))))
;;              (function    . ,(all-the-icons-nerd-mdi "function"                    :face `(:inherit (an-old-hope-yellow-int ,parent-face))))
;;              (constructor . ,(all-the-icons-nerd-mdi "dna"                         :face parent-face))
;;              (field       . ,(all-the-icons-nerd-mdi "code-parentheses"            :face parent-face))
;;              (variable    . ,(all-the-icons-nerd-mdi "cube-outline"                :face parent-face))
;;              (class       . ,(all-the-icons-nerd-mdi "code-braces"                 :face `(:inherit (an-old-hope-red ,parent-face))))
;;              (interface   . ,(all-the-icons-nerd-mdi "arrow-down-bold-box-outline" :face parent-face))
;;              (module      . ,(all-the-icons-nerd-mdi "package-variant-closed"      :face parent-face))
;;              (property    . ,(all-the-icons-nerd-oct "code"                        :face parent-face))
;;              (unit        . ,(all-the-icons-nerd-mdi "code-brackets"               :face parent-face))
;;              (value       . ,(all-the-icons-nerd-mdi "numeric"                     :face parent-face))
;;              (enum        . ,(all-the-icons-nerd-fa  "sort-alpha-asc"              :face `(:inherit (an-old-hope-orange ,parent-face))))
;;              (keyword     . ,(all-the-icons-nerd-mdi "apple-keyboard-shift"        :face parent-face))
;;              (snippet     . ,(all-the-icons-nerd-mdi "subdirectory-arrow-left"     :face parent-face))
;;              (color       . ,(all-the-icons-nerd-mdi "palette"                     :face parent-face))
;;              (file        . ,(all-the-icons-nerd-mdi "file"                        :face parent-face))
;;              (reference   . ,(all-the-icons-nerd-mdi "format-list-bulleted"        :face parent-face)))))
;;
;;  "Association between `eglot' LSP kinds and annotation icons for `company-mode'.
;;To reduce the amount of redundant processing in the margin function, this is defined
;;as a cons cell of icon alists, with the car alist being for regular candidates in the
;;company popup and the cdr alist for the current-candidate.
;;
;;This structure means you don't have to do any processing, or propertising to pick an
;;icon for a candidate. A simple alist lookup is all you need... it might be worth
;;turning this into a hashset.")

(defconst all-the-icons-default-completion-icon+
  (cons (all-the-icons-faicon "leaf" :face 'company-icon+)
        (all-the-icons-faicon "leaf" :face 'company-current-icon+)))

;; (setq company-format-margin-function
;;       (defun company-format-margin-function+ (candidate selected)
;;         (concat
;;          (make-string company-tooltip-margin ? )
;;          (or
;;           (when-let ((kind (company-call-backend 'kind candidate)))
;;             (alist-get kind
;;                        (if selected
;;                            (cdr all-the-icons-lsp-kinds+)
;;                          (car all-the-icons-lsp-kinds+))))
;;           (if selected
;;               (cdr all-the-icons-default-completion-icon+)
;;             (car all-the-icons-default-completion-icon+))))))

(require 'keybinds)
(provide 'config)
;;; core.el ends here
