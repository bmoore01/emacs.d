;; ui-funcs.el
(provide 'ui-funcs)

(defun pretty-mac-titlebar ()
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq-default ns-use-proxy-icon nil)
  (setq frame-title-format nil))

(defun load-theme--disable-old-theme (theme &rest args)
  "Disable current theme before loading the new one"
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)


;;;###autoload
(defun ui-cycle-themes ()
  (interactive)
  (setq ui-themes-to-cycle (nconc (last ui-themes-to-cycle) (butlast ui-themes-to-cycle)))
  (load-theme (car ui-themes-to-cycle) t nil))

;; TODO
;;(defun set-font (name size)
;;  (let ((font-name (concat name "-" (number-to-string size))))
;;  (add-to-list 'default-frame-alist '(font . font-name))
;;  (add-to-list 'default-frame-alist '(height . 24)
;;  (add-to-list 'default-frame-alist '(width . 80)))))
