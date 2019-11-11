;;; org-mode.el --- Summary
;;; Commentary:
;;; Code:
(require 'org-mode-funcs)
(require 'org-mode-keybinds)

(use-package org
  :commands (org-clock-out org-occur-in-agenda-files org-agenda-files)
  :init
  (org-setup-headers)
  :config
  (setq-default
   org-adapt-indentation nil
   org-adapt-dim-blocked-tasks nil
   org-agenda-files (list "~/Dropbox/org/agenda/calendar.org")
   org-latex-create-formula-image-program 'dvipng
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files nil
   org-cycle-include-plain-lists t
   org-cycle-separator-lines 1
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hide-emphasis-markers t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-indent-mode-turns-on-hiding-stars t
   org-pretty-entities t
   org-pretty-entities-include-sub-superscripts t
   org-startup-folded t
   org-startup-with-inline-images t
   org-use-sub-superscripts t
   org-src-tab-acts-natively t
   ontline-blank-line t
   org-startup-with-inline-images t
   org-image-actual-width nil
   org-confirm-babel-evaluate nil
   org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED" "DELEGATED")))

  (setq org-startup-with-inline-images t
	org-imenu-depth 8
	org-ellipsis "⤵")
  :hook
  (org-mode . (lambda ()
		     (progn
		       (setq left-margin-width 4
			     right-margin-width 4)
			     header-line-format " ")
		       (set-window-buffer nil (current-buffer))
		       (org-indent-mode)
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
