;;; util-funcs.el --- general utils for managing the config itself -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(defvar modules-dir)
(defvar langs-dir)

(defun create-module-file (module-name &optional type lang)
  "Create a template file for a module with the name MODULE-NAME TYPE.
MODULE-NAME can be set to \"keybinds\" or \"funcs\" if LANG is set to non-nil value will create template file in the langs directory."
  (let* ((file-name (if (eq type nil)
			module-name
		      (concat module-name "-" type)))
	 (module-path (if lang
			  (concat langs-dir module-name "/")
			(concat modules-dir module-name "/")))
	 (file-with-path (concat module-path file-name ".el")))

    (unless (file-exists-p module-path)
      (make-directory module-path))

    (with-temp-file file-with-path
      (insert (format ";;; %s.el --- Summary -*- lexical-binding:t -*-\n;;; Commentary:\n" file-name))

      (when (or (eq type "keybinds") (eq type "funcs"))
    	(insert (format ";;; All the %s for the %s module.\n" type module-name)))

      (insert ";;; Code:\n")
      (when (eq type nil)
	(insert (format "(require '%s-funcs)\n" file-name))
	(insert (format"(require '%s-keybinds)\n" file-name)))

      (insert "\n\n;;; Insert code here\n\n")

      (insert (format "(provide '%s)\n;;; %s.el ends here" file-name file-name))
      file-with-path)))

(defun create-module (module-name &optional lang)
  "Create all the templates nessecary for creating a module withe name MODULE-NAME if LANG is non-nil will create a language module."
  (progn
    (create-module-file module-name nil lang)
    (create-module-file module-name "keybinds" lang)
    (create-module-file module-name "funcs" lang)))

(defun new-module ()
  "Create a new module template and add to load path."
  (interactive)
  (let ((module-name (read-from-minibuffer "New module name: ")))
    (create-module module-name nil)
    (add-module module-name)
    (message (format "Added module: %s" module-name))))

(defun new-lang-module ()
  "Create a new lang module template."
  (interactive)
  (let ((module-name (read-from-minibuffer "New lang module name: ")))
    (create-module module-name t)
    (add-lang-module module-name)
    (message (format "Added lang module: %s" module-name))))

(provide 'utils-funcs)
;;; util-funcs.el ends here
