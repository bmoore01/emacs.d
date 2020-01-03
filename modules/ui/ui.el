;;; ui.el --- Summary
;;; All interface related packages and configuration
;;; Commentary:
;;; Code:

(require 'ui-funcs)
(require 'ui-keybinds)

;; set ui defaults
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; quiet startup
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(if (eq system-type 'darwin)
    (pretty-mac-titlebar))

;; set default font
;;(when (member "Menlo" (font-family-list))
;;  (set-frame-font "menlo-13:weight=regular" t t))
(when (member "Source Code Variable" (font-family-list))
  (set-frame-font "Source Code Variable-13:weight=regular" t t))

(use-package doom-themes)
(use-package all-the-icons)
(use-package all-the-icons-ivy
  :hook (after-init. all-the-icons-ivy-setup))
(use-package doom-modeline :config (doom-modeline-mode))

;;(use-package centaur-tabs
;;  :demand
;;  :init (setq centaur-tabs-set-bar 'under)
;;  :config
;;  (centaur-tabs-mode +1)
;;  (centaur-tabs-headline-match)
;;  (setq centaur-tabs-set-modified-marker t
;;        centaur-tabs-modified-marker " ● "
;;        centaur-tabs-cycle-scope 'tabs
;;        centaur-tabs-height 30
;;        centaur-tabs-set-icons t
;;        centaur-tabs-close-button " × ")
;;  (centaur-tabs-change-fonts "Arial" 130)
;;  (centaur-tabs-group-by-projectile-project)
;;  :bind
;;  ("C-S-<tab>" . centaur-tabs-backward)
;;  ("C-<tab>" . centaur-tabs-forward))

(use-package dashboard
  :hook
  (after-init . dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-items '((recents . 5)
		     (projects . 5))))


(setq ui-themes-to-cycle '(doom-moonlight doom-solarized-light))

(load-theme 'doom-moonlight t nil)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)

(provide 'ui)
;;; ui.el ends here
