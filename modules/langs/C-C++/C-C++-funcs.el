;;; C-C++-funcs.el --- Summary
;;; Commentary:
;;; Code:

(defun setup-dap-nodes ()
  "Setup dap node and and download if needed for debugging."
  (require 'dap-node)
  (dap-node-setup))

(provide 'C-C++-funcs)
;;; C-C++-funcs.el ends here
