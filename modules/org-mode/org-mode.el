;;; org-mode.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'org-mode-funcs)
(require 'org-mode-keybinds)

(use-package org
  :pin org
  :ensure org-plus-contrib
  :commands (org-capture org-agenda)
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " "
	org-agenda-start-with-log-mode t
	org-log-done 'time
	org-log-into-drawer t
	org-refile-targets '(("todo.org" :maxlevel . 1)
			     ("archive.org" :maxlevel . 1))
	org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/Dropbox/org/todo.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)


	org-startup-folded t
	org-enforce-todo-dependencies t
	org-startup-with-inline-images t
	org-use-sub-superscripts t
	org-indent-mode-turns-on-hiding-stars t
	org-preview-latex-default-process 'dvipng
	org-directory "~/Dropbox/org"
	org-agenda-files '("todo.org")
	org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "BLOCKED(b@/!)" "|" "DONE(d)" "CANCELLED(c@/!)"))
	org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
   					     ("BLOCKED" ("BLOCKED" . t))
   					     (done ("BLOCKED") ("HOLD"))
   					     ("TODO" ("BLOCKED") ("CANCELLED") ("HOLD"))
   					     ("NEXT" ("BLOCKED") ("CANCELLED") ("HOLD"))
   					     ("DONE" ("BLOCKED") ("CANCELLED") ("HOLD")))))
  (org-font-setup)))


(advice-add 'org-refile :after 'org-save-all-org-buffers)


(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))


(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(provide 'org-mode)
;;; org-mode.el ends here
