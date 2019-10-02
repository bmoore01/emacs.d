;; init.el

;; load the core of my config
(add-to-list 'load-path "~/.emacs.d/core/")
(require 'core)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "66d53738cc824d0bc5b703276975581b8de2b903d6ce366cd62207b5dd6d3d13" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "26e4be1b53973c7537ab090b578a49a538cc5820a060bb3d7155e64f523c0c7a" "66af8344f8a405dd07014827c2be17f6f91fcc1fd3a58966741e30551d2d3b5e" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" default)))
 '(package-selected-packages
   (quote
    (evil-org company flycheck counsel-projectile wgrep ag projectile counsel swiper ivy doom-modeline spaceline-colors spaceline-all-the-icons spaceline powerline all-the-icons doom-themes web-mode elixir-mode multi-term eterm-256color exec-path-from-shell neotree treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs evil general which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
