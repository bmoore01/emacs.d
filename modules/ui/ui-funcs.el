;;; ui-funcs.el --- All fucntions for changing the UI or utilities -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(defvar ui-themes-to-cycle)

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


;;;###autoload
(defun ui-cycle-themes ()
  "Cycle through the dark and light themes in the UI-THEMES-TO-CYCLE list."
  (interactive)
  (setq ui-themes-to-cycle (nconc (last ui-themes-to-cycle) (butlast ui-themes-to-cycle)))
  (load-theme (car ui-themes-to-cycle) t nil))

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

;;(defun adjust-font-size (increment)
;;(defun global-text-scale-adjust (increment)
;;  "Increase size of font in FRAME by INCREMENT.
;;FRAME parameter defaults to current frame."
;;  (let* ((font (frame-parameter nil 'font))
;;	 (font default-font)
;;	 (increment (* increment 2))
;;	 (zoom-factor (or font-scale 0)))
;;    (let ((new-size (+ (string-to-number (aref font xlfd-regexp-pixelsize-subnum))
;;		       increment)))
;;      (unless (> new-size 0)
;;	(error "Font is too small at %d" new-size)))
;;      ;;(aset font xlfd-regexp-pixelsize-subnum (number-to-string new-size)))
;;    ;; Set point size & width to "*", so frame width will adjust to new font size
;;    (aset font xlfd-regexp-pointsize-subnum "*")
;;    (aset font xlfd-regexp-avgwidth-subnum "*")
;;    (setq font (x-compose-font-name font))
;;    (unless (x-list-fonts font)
;;      (error "Cannot change font size"))
;;    (set-frame-font font 'keep-size t)
;;    (setf (alist-get 'font default-frame-alist) font)))
;;    ;; (setq doom--font-scale (+ zoom-factor increment))
;;    ;; Unlike `set-frame-font', `set-frame-parameter' won't trigger this
;;    ;; (run-hooks 'after-setting-font-hook)))


(defvar +modeline--old-bar-height nil)
;;;###autoload
(defun modeline-resize-for-font (inc)
  (unless +modeline--old-bar-height
    (setq +modeline--old-bar-height doom-modeline-height))
  (let ((default-height +modeline--old-bar-height)
        (scale (or (frame-parameter nil 'font-scale) 0)))
    (setq doom-modeline-height
          (if (> scale 0)
              (+ default-height (* scale (* 2 inc)))
            default-height))))

(provide 'ui-funcs)
;;; ui-funcs.el ends here
