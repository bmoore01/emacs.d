;;; core-lib.el -- Globally availalable functions -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:
(require 'cl-lib)

(defun remove-sublist-from-list (to-remove lst)
  "Remove all members of `TO-REMOVE' form `LST'.  For exmpale (remove-sublist-from list '(a b c) '(a b c d e f)) => '(d e f)."
  (cl-remove-if (lambda (x) (member x to-remove)) lst))

(defun safe-concat (x y)
  "If X and Y are non nil concat them otherwise return X."
  (if (and x y)
      (concat x y)
    x))

(defun clean-dir-files (path)
  "Return contents of a directory at PATH without . and .."
  (remove-sublist-from-list '(".." ".") (directory-files path)))


(defun current-buffer-major-mode ()
  "Returns the major mode of the current buffer as a symbol."
  (with-current-buffer (current-buffer) major-mode))

(cl-defmacro create-leader-key-prefix (name &rest params &key which-key &allow-other-keys)
  "Just a wrapper for `general-create-definer' for simpler which-key naming.
`NAME' is the name of the definer and `PARAMS' is everything else and
`WHICH-KEY' is the name of the prefix in which-key buffer"
  (declare (indent defun))
  (let ((other-keys (cl-loop for (k v) on params by 'cddr
			      unless (eq k :which-key)
			      collect k
			      and collect v)))
    `(general-create-definer
      ,name
      ,@other-keys
      "" (quote (:ignore t :which-key ,which-key)))))

(provide 'core-lib)
;;; core-lib.el ends here
