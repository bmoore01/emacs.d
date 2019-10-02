;; ui.el
(provide 'ui)
(require 'ui-funcs)
(require 'ui-keybinds)

;; set ui defaults
(tool-bar-mode -1)
(scroll-bar-mode -1)

(if (eq system-type 'darwin)
    (pretty-mac-titlebar))

;; set default font
(when (member "IBM Plex Mono" (font-family-list))
  (add-to-list 'default-frame-alist '(font ."IBM Plex Mono-16"))
  (add-to-list 'default-frame-alist '(width . 80)))


(use-package doom-themes)
(use-package all-the-icons)
(use-package doom-modeline :config (doom-modeline-mode))


(setq ui-themes-to-cycle '(doom-moonlight doom-solarized-light))

(load-theme 'doom-moonlight t nil)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)
