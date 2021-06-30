;;; ui-funcs.el --- All fucntions for changing the UI or utilities -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:
(defun pretty-mac-titlebar ()
  "Configuration to hide MacOS titlebar when in windowed mode."
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq-default ns-use-proxy-icon nil)
  (setq frame-title-format nil))

(defun load-theme--disable-old-theme (theme &rest args)
  "Disable current THEME before loading the new one ARGS are ignored."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)

(defvar ui-after-theme-cycle-hook nil
  "Hook called after theme is switched from the ui-cycle-themes method.")

;;;###autoload
(defun ui-cycle-themes ()
  "Cycle through the dark and light themes in the UI-THEMES-TO-CYCLE list."
  (interactive)
  (setq ui-themes-to-cycle (nconc (last ui-themes-to-cycle) (butlast ui-themes-to-cycle)))
  (load-theme (car ui-themes-to-cycle) t nil)
  (run-hooks 'ui-after-theme-cycle-hook))

;;;###autoload
(defun set-font (font size weight)
  "Set the FONT SIZE and WEIGHT of the default font for all windows."
  (let ((font-str (concat font "-" (number-to-string  size) ":weight=" weight)))
    (when (member font (font-family-list))
      (set-frame-font font-str t t))))

(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

;;;###autoload
(defun global-text-scale-adjust (inc)
  "Increase or decreacse the font size by 1 in all windows depending on INC."
  (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1)
  (modeline-resize-for-font inc))

(provide 'ui-funcs)
;;; ui-funcs.el ends here
