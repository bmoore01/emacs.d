;;; core-funcs.el -- Vial functions for conf -*- lexical-binding:t -*-
;;; functions required for the initialization and compilation of my emacs config
;;; Commentary:
;;; need to refactor `add-all-modules' to be able to exclude modules to avoid janky implementation
;;; then can create macro for the init file which shows which files to include
;;; Code:

;; defvars to avoid free vaible error
(defvar modules-dir)
(defvar langs-dir)
(defvar scratch-mode)

(defun setup-use-package ()
  "Setup the package archives and install use-package."
  (require 'package)
  (setq package-enable-at-startup nil)
  (setq package-archives '(("org"       . "http://orgmode.org/elpa/")
				   ("gnu"       . "http://elpa.gnu.org/packages/")
				   ("melpa-stable"     . "http://stable.melpa.org/packages/")
				   ("melpa"     . "http://melpa.milkbox.net/packages/")
				   ("marmalade" . "http://marmalade-repo.org/packages/")))

  (package-initialize)
  (unless (package-installed-p 'use-package) ; unless it is already installed
    (package-refresh-contents) ; updage packages archive
    (package-install 'use-package)) ; and install the most recent version of use-package
  (require 'use-package))


;; consider refactoring out helper functions into a core utils module that just sets up my own elisp stuff before starting the config
;; consider even putting a small subset of core global functions in early-init.el
(defun remove-sublist-from-list (to-remove lst)
  "Remove all members of `TO-REMOVE' form `LST'.  For exmpale (remove-sublist-from list '(a b c) '(a b c d e f)) => '(d e f)."
  (cl-remove-if (lambda (x) (member x to-remove)) lst))

(defun concat-not-nil (x y)
  "If X and Y are non nil concat them otherwise return X."
  (if (and x y)
      (concat x y)
    x))

(defun clean-dir-files (path)
  "Return contents of a directory at PATH without . and .."
  (remove-sublist-from-list '(".." ".") (directory-files path)))

(defun recompile-config-modules ()
  "Byte compile everything in the `~/.emacs.d/modules/' directory."
  (interactive)
  (let ((prefix "~/.emacs.d/modules/"))
    (mapcar (lambda (x)
	      (byte-recompile-directory (concat prefix x) 0))
	    (clean-dir-files prefix))))


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


;;;###autoload
(defun add-module (module-name &optional dir)
  "Add module `MODULE-NAME 'to the `load-path' if `DIR' is set use specified dir otherwise use `modules-dir'."
  (if dir
      (add-to-list 'load-path (concat dir module-name))
    (add-to-list 'load-path (concat modules-dir module-name)))
  (require (intern module-name)))

;;;###autoload
(defun add-lang-module (module-name)
  "Add lang module `MODULE-NAME 'to the `load-path'."
  (add-module module-name langs-dir))

;;;###autoload
(defun add-all-modules (&optional excluded)
  "Add all modules in the `modules-dir' to the `load-path', execpt any listed in EXCLUDED."
  (mapc (lambda (x) (add-module x))
	  ;; don't include langs twice add utils manually
	  (remove-sublist-from-list '("langs" "utils") (clean-dir-files modules-dir)))
  (add-to-list 'load-path "util-funcs"))

;;;###autoload
(defun add-langs ()
  "Add all modules in the `langs-dir' to the `load-path'."
  (mapcar (lambda (x) (add-lang-module x))
	  (clean-dir-files langs-dir)))

(defun raise-gc-on-init ()
  "Set garbage collection to be higher on initialisation."
  (setq gc-cons-threshold 50000000)
  (add-hook 'emacs-startup-hook (lambda ()
	      (setq gc-cons-threshold 800000))))


;; TODO: Add regex to skip any buffer surrounded by asterisks
(defvar my-skippable-buffers '("*Messages*" "*Completions*" "*Help*" "*Buffer List*" "Shell-popup")
  "Buffer names ignored by `next-buffer' and `previous-buffer'.")

(defun my-buffer-predicate (buffer)
  "Tell `next-buffer' and `previous-buffer' to skip the BUFFER if its name is listed in `my-skippable-buffers'."
  (if (member (buffer-name buffer) my-skippable-buffers)
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

;;; shamelessly stolen from shell-pop
(defun create-popup--calculate-window-size (popup-window-size)
  "Calculates height for window taking `POPUP-WINDOW-SIZE' which is a percentage and returning the actual height of the window."
  (let* ((win (frame-root-window))
         (size (window-height win)))
    (round (* size (/ (- 100 popup-window-size) 100.0)))))

(defun create-popup (fname popup-func height select)
  "Created a popup window of height HEIGHT which is stored in FNAME, will call create buffer BUF-NAME and call POPUP-FUNC in the new window."
  (if (not (get fname 'state))
      (let ((win (split-window (frame-root-window) (create-popup--calculate-window-size height))))
	(when select
	  (select-window win))
	(funcall popup-func)
	(put fname 'state win))
    (progn
      (let ((win (get fname 'state)))
	(delete-window win)
	(put fname 'state nil)))))

(defun shell-toggle ()
  "Open a dumb shell in a popup in a buffer."
  (interactive)
  (if (not (get :shell-toggle 'state))
      (let* ((buffer (get-buffer-create "Shell-popup"))
	     (win (display-buffer-in-side-window buffer `((window-height . 12)))))
	(shell buffer)
	(set-frame-font "MesloLGS NF")
	(put :shell-toggle 'state win))
    (progn
      (let ((win (get :shell-toggle 'state)))
	(delete-window win)
	(put :shell-toggle 'state nil)))))

(defun smart-shell-toggle ()
  "Open a smarter shell using term mode in a popup window."
  (interactive)
  (if (not (get :smart-shell-toggle 'state))
      (let* ((buffer (get-buffer-create "Terminal"))
	     (win (display-buffer-in-side-window buffer `((window-height . 12)))))
	(ansi-term "/bin/zsh" buffer)
	(put :smart-shell-toggle 'state win))
    (progn
      (let ((win (get :smart-shell-toggle 'state)))
	(delete-window win)
	(put :smart-shell-toggle 'state nil)))))
(add-hook 'term-mode-hook (lambda () (set-frame-font "MesloLGS NF")))

(defun eshell-toggle ()
  "Togle the opening of eshell popup-window."
  (interactive)
  (create-popup
   'eshell-toggle 'eshell pop-toggle-size t))

(defun elisp-repl-toggle ()
  "Togle the opening of ielm repl popup-window."
  (interactive)
  (create-popup
   'ielm-toggle 'ielm pop-toggle-size t))

(defun initialise-core ()
  "Start the configuration."
  (progn
    (raise-gc-on-init)
    (setup-use-package)))

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

(provide 'core-funcs)
;;; core-funcs.el ends here
