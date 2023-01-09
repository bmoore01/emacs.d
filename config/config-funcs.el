;;; config-funcs.el -- Functions for config -*- lexical-binding:t -*-
;;; 
;;; Commentary:
;;; Put functions in a separate file to cleanup my config
;;; Code:

;; defvars to avoid free variable error
(defvar scratch-mode)

(defun setup-use-package ()
  "Setup the package archives and install use-package."
  (require 'package)
  (setq package-enable-at-startup nil
	use-package-always-ensure t) ;; So I dont need to put :ensure t in every package
  (setq package-archives '(("org" . "http://orgmode.org/elpa/")
			   ("gnu" . "http://elpa.gnu.org/packages/")
			   ("melpa-stable" . "http://stable.melpa.org/packages/")
			   ("melpa" . "http://melpa.org/packages/")))

  (package-initialize)
  (unless (package-installed-p 'use-package) ; unless it is already installed
    (package-refresh-contents) ; updage packages archive
    (package-install 'use-package)) ; and install the most recent version of use-package
  (require 'use-package))

(defun new-empty-buffer ()
  "Create a new buffer called untitled(<n>)."
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer or create it if needed."
  (interactive)
  (let ((exists (get-buffer "*scratch*")))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (when (and (not exists)
	       (not (eq major-mode scratch-mode))
	       (fboundp scratch-mode))
      (funcall scratch-mode))))

(defun add-path-string-to-exec-path (str)
  "Add all paths in STR seperated by colons to the exec path."
  (mapcar (lambda (s) (setenv "PATH" (concat s))) (split-string str ":")))

(defun emacs-special-buffer-name-p (name)
  "If NAME is surrounded by asterisks return t otherwise return nil."
  (if (string-match-p "\\*.*\\*" name)
      t))

(defvar my-skippable-buffers '("Shell-popup")
  "Buffer names ignored by `next-buffer' and `previous-buffer'.")

(defun my-buffer-predicate (buffer)
  "Tell `next-buffer' and `previous-buffer' to skip the BUFFER if it's name is listed in `my-skippable-buffers' or is an EMACS special buffer."
  (if (or (emacs-special-buffer-name-p (buffer-name buffer)) (member (buffer-name buffer) my-skippable-buffers))
      nil
    t))
(set-frame-parameter nil 'buffer-predicate 'my-buffer-predicate)


;; stolen right out of spacemacs
(defun neotree-expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
	  (progn
	    (neo-buffer--set-expand node t)
	    (neo-buffer--refresh t)
	    (when neo-auto-indent-point
	      (next-line)
	      (neo-point-auto-indent)))
	(call-interactively 'neotree-enter)))))

;; neotree funcs
(defun neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
	(neo-buffer--set-expand node nil)
	(neo-buffer--refresh t))
      (when neo-auto-indent-point
	(neo-point-auto-indent)))))

(defun neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent mode."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
	  (if (neo-buffer--expanded-node-p node)
	      (neotree-collapse))
	(neotree-select-up-node))
      (neotree-select-up-node))))

(defun split-window-and-follow-horizontally ()
  "Split window right and select new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-window-and-follow-vertically ()
  "Split window below and select new window."
  (interactive)
  (split-window-below)
  (other-window 1))

;;;###autoload
(defun my-relative-linum-toggle ()
  "Allow for toggling relative line number mode without re-initialising line number mode."
  (interactive)
  (if (not (get :my-linum-toggle 'state))
      (progn
	(linum-relative-toggle)
	(put :my-linum-toggle 'state t))
    (linum-relative-toggle)
    (display-line-numbers-mode)
    (put :my-linum-toggle 'state nil)))

(provide 'config-funcs)
;;; config-funcs.el ends here
