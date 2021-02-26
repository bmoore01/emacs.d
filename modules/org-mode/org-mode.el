;;; org-mode.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'org-mode-funcs)
(require 'org-mode-keybinds)

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(use-package org
  :commands (org-clock-out org-occur-in-agenda-files org-agenda-files)
  :hook
  (org-mode . org-mode-setup)
  :config
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) ""))
  (setq
   org-agenda-files (list "~/Dropbox/org/agenda.org")
   org-default-notes-file "~/Dropbox/org/refile.org"
   org-directory "~/Dropbox/org"
   org-latex-create-formula-image-program 'dvipng
   org-agenda-skip-unavailable-files nil
   org-cycle-separator-lines 1
   org-fontify-done-headline t
   org-fontify-whole-heading-line t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label t
   org-hide-emphasis-markers t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-indent-mode-turns-on-hiding-stars t
   org-pretty-entities t
   org-pretty-entities-include-sub-superscripts t
   org-startup-folded t
   org-enforce-todo-dependencies t
   org-startup-with-inline-images t
   org-use-sub-superscripts t
   org-src-tab-acts-natively t
   org-src-fontify-natively t
   ;; org-refile-use-outline-path 'file
   ;;org-ellipsis "⤵"
   org-ellipsis ""
   org-confirm-babel-evaluate nil
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "BLOCKED(b@/!)" "|" "DONE(d)" "CANCELLED(c@/!)"))
   org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
   					("BLOCKED" ("BLOCKED" . t))
   					(done ("BLOCKED") ("HOLD"))
   					("TODO" ("BLOCKED") ("CANCELLED") ("HOLD"))
   					("NEXT" ("BLOCKED") ("CANCELLED") ("HOLD"))
   					("DONE" ("BLOCKED") ("CANCELLED") ("HOLD"))))))

;;(use-package writeroom-mode
;;  :defer t
;;  :config
;;    (setq writeroom-width 140
;;          writeroom-mode-line nil
;;          writeroom-global-effects '(writeroom-set-bottom-divider-width
;;                                     writeroom-set-internal-border-width
;;                                     (lambda (arg)
;;                                       (let ((langs '("python"
;;                                                      "emacs-lisp"
;;                                                      "common-lisp"
;;                                                      "js"
;;                                                      "ruby")))
;;                                         (cond
;;                                          ((= arg 1)
;;                                           (progn
;;                                             (setq org-src-block-faces
;;                                                   (mapcar (lambda (lang) (list lang '(:family "Source Code Pro" :height 0.8))) langs))
;;                                             (normal-mode)
;;                                             (variable-pitch-mode)))
;;                                          ((= arg -1)
;;                                           (progn
;;                                             (setq org-src-block-faces
;;                                                   (mapcar (lambda (lang) (list lang '(:family "Source Code Pro" :height 1.0))) langs))
;;                                             (normal-mode)
;;                                             (variable-pitch-mode)
;;                                             (variable-pitch-mode)))))))))

(use-package evil-org
  :ensure t
  :hook (org-mode . evil-org-mode)
  :hook (lambda ()
	  (evil-org-set-key-theme))
  :config
  (progn
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(provide 'org-mode)
;;; org-mode.el ends here
