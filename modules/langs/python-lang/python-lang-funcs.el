;;; python-lang-funcs.el --- Summary
;;; Commentary:
;; Will need rewriting if I redo create-popup which i'd really like to
;;; Code:

(defmacro check-python-running (func)
  "If Python process is not running, start it then call FUNC otherwise just call FUNC."
  (if (not (get-buffer "*Python*"))
      (run-python))
  (funcall func))

;;;###autoload
(defun get-pyshell-buffer ()
  "If Python process is not running, start it then return the buffer."
  (if (not (get-buffer "*Python*"))
      (run-python)
    (pop-to-buffer-same-window "*Python*")))

(defun python-popup-toggle ()
  "Togle the opening of python repl popup-window."
  (interactive)
  (create-popup
   'python-tggle 'get-pyshell-buffer pop-toggle-size t))

(provide 'python-lang-funcs)
;;; python-lang-funcs.el ends here
