;; html.el html and all similar formats
(provide 'html)

(use-package web-mode)

(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
