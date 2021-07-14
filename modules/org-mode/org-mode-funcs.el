;;; org-mode-funcs.el --- Summary
;;; Commentary:
;;; Code:

(defun org-mode-setup()
  "Set up org mode to behave more like a word processor."
  (org-indent-mode)
  (setq header-line-format " ")
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun org-font-setup ()
  "Setup org mode font faces."
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.25)
                  (org-level-4 . 1.2)
                  (org-level-5 . 1.3)
                  (org-level-6 . 1.3)
                  (org-level-7 . 1.3)
                  (org-level-8 . 1.3)))
   (set-face-attribute (car face) nil :font "IBM Plex Sans" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(provide 'org-mode-funcs)
;;; org-mode-funcs.el ends here
