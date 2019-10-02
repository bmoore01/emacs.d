;; ide.el

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-project-search-path '("~/workspace")
	projectile-completion-system 'ivy))

(use-package ag)
(use-package wgrep)
(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package company
  :hook (prog-mode . global-company-mode))

(require 'ide-keybinds)
(provide 'ide)
;;; ide.el ends here
