;;; org-mode.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'org-mode-funcs)
(require 'org-mode-keybinds)

(use-package org
  :commands (org-clock-out org-occur-in-agenda-files org-agenda-files)
  :init
  (org-setup-headers)
  :config
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) ""))
  (setq
   org-adapt-dim-blocked-tasks nil
   org-agenda-files (list "~/Dropbox/org/agenda/calendar.org")
   org-default-notes-file "~/Dropbox/org/refile.org"
   org-directory "~/Dropbox/org"
   org-latex-create-formula-image-program 'dvipng
   org-agenda-skip-unavailable-files nil
   org-cycle-include-plain-lists 'integrate
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
   ontline-blank-line t
   org-refile-use-outline-path 'file
   org-imenu-depth 8
   org-ellipsis "⤵"
   org-confirm-babel-evaluate nil
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))
   org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
					("WAITING" ("WAITING" . t))
					("HOLD" ("WAITING") ("HOLD" . t))
					(done ("WAITING") ("HOLD"))
					("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
					("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
					("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  :hook
  (org-mode . (lambda ()
		     (progn
		       (setq left-margin-width 4
			     right-margin-width 4)
			     header-line-format " ")
		       (set-window-buffer nil (current-buffer))
		       (org-indent-mode)
		       (fringe-mode 0)
		       (visual-line-mode))))


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
