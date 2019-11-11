;;; go-funcs.el --- Summary
;;; Commentary:
;;; Code:


(defun my-go-mode-hook ()
  "Add autosave and default compile command in go mode."
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(provide 'go-funcs)
;;; go-funcs.el ends here
