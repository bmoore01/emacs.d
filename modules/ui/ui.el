;;; ui.el --- Summary -*- lexical-binding:t -*-
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

;; add window divider
(setq window-divider-default-bottom-width 1
      window-divider-default-places 'bottom-only)

;; split windows for popups vertically
(setq split-width-threshold nil)

(if (eq system-type 'darwin)
    (pretty-mac-titlebar))

(defvar default-font "Roboto mono")
(defvar default-font-size 18)
(defvar font-size-increment 1)
(defvar font-scale 1)
(defvar default-font-weight "regular")

(use-package all-the-icons)
(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package git-gutter
  :ensure git-gutter-fringe
  :hook (prog-mode . git-gutter-mode))

(use-package rainbow-mode
  :defer t)

(use-package dashboard
  :hook
  (after-init . dashboard-setup-startup-hook)
  :custom

  (dashboard-set-heading-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-items '((recents . 5)
		     (projects . 5))))

(use-package doom-themes)
(use-package all-the-icons)
(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package doom-modeline
  :config (doom-modeline-mode))

(defvar ui-themes-to-cycle '(doom-moonlight spacemacs-light))

(load-theme 'doom-moonlight t nil)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)

(provide 'ui)
;;; ui.el ends here
