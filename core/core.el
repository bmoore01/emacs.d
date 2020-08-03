;;; core.el --- The loader of modules -*- lexical-binding:t -*-
;;; Commentary:
;;; Might factor window popup into a seperate package for use at some point but until then it's going to live in here.
;;; Code:
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
 show-paren-delay 0
 evil-want-C-u-scroll t)

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

(defvar use-package-always-ensure t)

(defvar scratch-mode 'lisp-interaction-mode
  "Default major mode of the scratch buffer.")

;; highlight parens
(show-paren-mode 1)

;; show line numbers only in code buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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

(use-package hydra)

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
	ivy-count-format "(%d/%d) "))

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
           ;;((ivy-rich-switch-buffer-icon (:width 2))
           ;; (ivy-rich-candidate (:width 35))
           ;; (ivy-rich-switch-buffer-project (:width 15 :face success))
           ;; (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
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
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel)
(use-package swiper)

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

(require 'core-keybinds)
(add-all-modules)
(add-langs)
(provide 'core)
;;; core.el ends here
