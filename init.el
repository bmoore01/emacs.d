;; init.el

;; load the core of my config
(add-to-list 'load-path "~/.emacs.d/core/")
(require 'core)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-lsp--cache-item-candidates t t)
 '(company-lsp--candidates-async t t)
 '(custom-safe-themes
   (quote
    ("728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "66d53738cc824d0bc5b703276975581b8de2b903d6ce366cd62207b5dd6d3d13" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "26e4be1b53973c7537ab090b578a49a538cc5820a060bb3d7155e64f523c0c7a" "66af8344f8a405dd07014827c2be17f6f91fcc1fd3a58966741e30551d2d3b5e" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" default)))
 '(dashboard-center-content t)
 '(dashboard-items (quote ((recents . 5) (projects . 5))) t)
 '(dashboard-set-file-icons t)
 '(dashboard-set-heading-icons t)
 '(dashboard-set-navigator t)
 '(dashboard-startup-banner (quote logo) t)
 '(lsp-auto-guess-root nil)
 '(lsp-log-io nil)
 '(lsp-prefer-flymake t)
 '(lsp-print-io nil)
 '(lsp-print-performance nil)
 '(lsp-trace nil t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-sideline-show-hover t)
 '(neo-auto-indent-point t)
 '(neo-create-file-auto-open t)
 '(neo-hide-cursor t)
 '(neo-smart-open t)
 '(package-selected-packages
   (quote
    (centaur-tabs ivy-posframe ivy-rich all-the-icons-ivy evil-numbers cargo-mode lsp-java go-direx go-mode org-bullets org-pretty-table evil-surround smartparens persp-mode dashboard dap-ui dap-mode shell-pop lsp-mode linum-relative evil-org company flycheck counsel-projectile wgrep ag projectile counsel swiper ivy doom-modeline spaceline-colors spaceline-all-the-icons spaceline powerline all-the-icons doom-themes web-mode elixir-mode multi-term eterm-256color exec-path-from-shell neotree treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs evil general which-key use-package)))
 '(shell-pop-full-span t)
 '(shell-pop-window-size 30))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
 '(org-document-title ((t (:inherit default :weight bold :background nil :font "Lucida Grande" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :background nil :font "Lucida Grande" :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight bold :background nil :font "Lucida Grande" :height 1.25))))
 '(org-level-3 ((t (:inherit default :weight bold :background nil :font "Lucida Grande" :height 1.1))))
 '(org-level-4 ((t (:inherit default :weight bold :background nil :font "Lucida Grande" :height 1))))
 '(org-level-5 ((t (:inherit default :weight bold :background nil :font "Lucida Grande"))))
 '(org-level-6 ((t (:inherit default :weight bold :background nil :font "Lucida Grande"))))
 '(org-level-7 ((t (:inherit default :weight bold :background nil :font "Lucida Grande"))))
 '(org-level-8 ((t (:inherit default :weight bold :background nil :font "Lucida Grande")))))
