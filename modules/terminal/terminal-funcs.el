;;; terminal-funcs.el --- Summary -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

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


(provide 'terminal-funcs)
;;; terminal-funcs.el ends here
