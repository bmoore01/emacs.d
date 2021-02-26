;;; ide-funcs -- Summary
;; functions to add additional ide-like functionality

;;; Commentary:

;;; Code:

;;;###autoload
(defun my-relative-linum-toggle ()
  "Allow for toggling relative line number mode without re-initialising line number mode."
  (interactive)
  (if (not (get :my-linum-toggle 'state))
      (progn
	(linum-relative-toggle)
	(put :my-linum-toggle 'state t))
    (linum-relative-toggle)
    (display-line-numbers-mode)
    (put :my-linum-toggle 'state nil)))

;;;###autoload
(defun my-toggle-lsp-ui-doc ()
  "A toggle for lsp ui doc if lsp ui doc is active disable it if it is disabled activate it."
  (interactive)
  (if lsp-ui-doc-mode
    (progn
      (lsp-ui-doc-mode -1)
      (lsp-ui-doc--hide-frame))
     (lsp-ui-doc-mode 1)))

;;;###autoload
(defun my-toggle-lsp-ui-imenu ()
  "A toggle for lsp ui-imenu doc if imenu is open close it if it is close open it."
  (interactive)
  (if (not (get :my-toggle-lsp-ui-imenu 'state))
      (progn
	(lsp-ui-imenu)
	(put :my-toggle-lsp-ui-imenu 'state t))
    (lsp-ui-imenu--kill)
    (put :my-toggle-lsp-ui-imenu 'state nil)))

(provide 'ide-funcs)
;;; ide-funcs.el ends here
