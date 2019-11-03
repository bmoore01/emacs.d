;; html.el html and all similar formats
(provide 'html)

(use-package web-mode
  :config
  (setq web-mode-enable-auto-closing t
	web-mode-enable-auto-opening t
	web-mode-enable-auto-indentation t))

(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
