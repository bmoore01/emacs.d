;;; org-mode-funcs.el --- Summary
;;; Commentary:
;;; Code:


(require 'org-tempo)

(defun org-mode-visual-fill ()
  "Set margins for org mode buffers."
 (setq-local visual-fill-column-width 150
	      visual-fill-column-center-text t)
  (visual-fill-column-mode t))

(defun org-mode-setup ()
  "Org mode setup function called in hook."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (git-gutter-mode -1) ;; disable git gutter mode for org buffers


  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))
  (add-to-list 'org-structure-template-alist '("jv" . "src java"))

  (org-babel-do-load-languages 'org-babel-load-languages '((java . t)))

  (nconc org-babel-default-header-args:java
	 '((:dir . nil)
           (:results . value)))


  (org--enable-regular-mode))


(defun org--enable-regular-mode ()
  (setq header-line-format nil)

  (thin-modeline-mode -1)

  (dolist (face '((org-level-1 . 1.6)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1)))
    (set-face-attribute (car face) nil :family "Cantarell" :weight 'normal :height (cdr face)))

  (set-face-attribute 'org-headline-done nil :inherit 'variable-pitch :family "Cantarell")

  (set-face-attribute 'org-document-title nil :foreground nil :inherit 'variable-pitch :family "Cantarell" :height 1.8 :underline nil)
  (set-face-attribute 'org-document-info nil :foreground nil :inherit 'variable-pitch :height 1.2 :slant 'italic)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch) :height 1.25)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(defun org--enable-fancy-mode ()
  ;; add header line without background
  (custom-set-faces `(header-line ((t (:background nil)))))
  (setq header-line-format " ")

  (thin-modeline-mode 1)

  ;; enable fancy fonts
  (dolist (face '((org-level-1 . 1.6)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1))                  )
    (set-face-attribute (car face) nil :family "EtBembo" :weight 'normal :height (cdr face)))


  (set-face-attribute 'org-headline-done nil :inherit 'variable-pitch :family "EtBembo" :strike-through t)

  (set-face-attribute 'org-document-title nil :foreground nil :inherit 'variable-pitch :family "EtBembo" :height 1.8 :underline nil)
  (set-face-attribute 'org-document-info nil :foreground nil :inherit 'variable-pitch :height 1.2 :slant 'italic)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch) :height 1.25)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(define-minor-mode org-fancy-mode
  "When enabled org is displayed as more of a writing environment."
  :init-value nil
  (if (and org-fancy-mode (eq (current-buffer-major-mode) 'org-mode))
      (org--enable-fancy-mode)
    (org--enable-regular-mode)))

(provide 'org-mode-funcs)
;;; org-mode-funcs.el ends here
