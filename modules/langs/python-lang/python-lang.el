;;; python-lang.el --- All things python3
;;; Commentary:
;;; This module is named this way because EMACS already has a python.el
;;; Code:
(require 'python-lang-funcs)
(require 'python-lang-keybinds)

(setq python-shell-interpreter "python3")

;;; Addresses the following bug
;;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25753#44
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(provide 'python-lang)
;;; python-lang.el ends here
