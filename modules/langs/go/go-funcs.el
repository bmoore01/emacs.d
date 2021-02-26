;;; go-funcs.el --- Summary
;;; Commentary:
;;; Code:


(defun go-mode-setup ()
  "Add autosave and default compile command in go mode."
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go lint")))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;;Smaller compilation buffer
(setq compilation-window-height 14)
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))

(add-hook 'compilation-mode-hook 'my-compilation-hook)

(provide 'go-funcs)
;;; go-funcs.el ends here
