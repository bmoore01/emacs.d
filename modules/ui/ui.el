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
(when (member "IBM Plex Mono" (font-family-list))
  (add-to-list 'default-frame-alist '(font ."IBM Plex Mono-16"))
  (add-to-list 'default-frame-alist '(width . 80)))


(use-package doom-themes)
(use-package all-the-icons)
(use-package doom-modeline :config (doom-modeline-mode))

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
