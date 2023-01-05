;; init.el

;; load the core of my config
(add-to-list 'load-path "~/.emacs.d/core/")
(require 'core)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212337" "#ff5370" "#c7f59b" "#ffbd76" "#70b0ff" "#baacff" "#34d3fb" "#e4f3fa"])
 '(company-lsp--cache-item-candidates t t)
 '(company-lsp--candidates-async t t)
 '(custom-safe-themes
   '("6c71a6437c3edf3fb28156ea83dbcf752fef19590fc1bfc919b0a53935f265d2" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "b69323309e5839676409607f91c69da2bf913914321c995f63960c3887224848" "054e929c1df4293dd68f99effc595f5f7eb64ff3c064c4cfaad186cd450796db" "0eb3c0868ff890b0c4ee138069ce2a8936a8a69ba150efa6bfb9fb7c05af5ec3" "f7b0f2d0f37846ef75157f5c8c159e6d610c3efcc507cbddec789c02e165c121" "3ee39fe8a6b6e0f1cbdfa33db1384bc778e3eff4118daa54af7965e9ab8243b3" "58c2c8cc4473c5973e77f4b78a68c0978e68f1ddeb7a1eb34456fce8450be497" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "66d53738cc824d0bc5b703276975581b8de2b903d6ce366cd62207b5dd6d3d13" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "26e4be1b53973c7537ab090b578a49a538cc5820a060bb3d7155e64f523c0c7a" "66af8344f8a405dd07014827c2be17f6f91fcc1fd3a58966741e30551d2d3b5e" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" default))
 '(dap-python-executable "python3")
 '(dashboard-center-content t)
 '(dashboard-items '((recents . 5) (projects . 5)))
 '(dashboard-set-file-icons t)
 '(dashboard-set-heading-icons t)
 '(dashboard-set-navigator t)
 '(dashboard-startup-banner 'logo)
 '(doom-modeline-height 25)
 '(fci-rule-color "#383e5c")
 '(global-text-scale-mode 1)
 '(jdee-db-active-breakpoint-face-colors (cons "#161a2a" "#baacff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#161a2a" "#c7f59b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#161a2a" "#383e5c"))
 '(lsp-auto-guess-root nil)
 '(lsp-log-io nil)
 '(lsp-prefer-flymake t t)
 '(lsp-print-performance nil)
 '(lsp-trace nil t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-sideline-show-hover t)
 '(neo-auto-indent-point t)
 '(neo-create-file-auto-open t)
 '(neo-hide-cursor t)
 '(neo-smart-open t)
 '(objed-cursor-color "#ff5370")
 '(org-agenda-files nil)
 '(package-selected-packages
   '(lsp-jedi company-emoji shades-of-purple-theme org-plus-contrib org mini-frame nasm-mode x86-lookup solidity-flycheck company-solidity flycheck-solidity solidity-mode writeroom-mode whiteroom-mode rainbow-mode visual-fill-column visual-fill spacemacs-theme evil-collection ivy-hydra company-box all-the-icons-ivy-rich js2-mode rjsx-mode tide all-the-icons-dired git-gutter-fringe evil-magit magit eyebrowse yasnippet ccls add-node-modules-path ivy-xref kaolin-themes cider centaur-tabs ivy-posframe ivy-rich all-the-icons-ivy evil-numbers cargo-mode lsp-java go-direx go-mode org-bullets org-pretty-table evil-surround smartparens persp-mode dashboard dap-ui dap-mode shell-pop lsp-mode linum-relative evil-org company flycheck counsel-projectile wgrep ag projectile counsel swiper ivy doom-modeline spaceline-colors spaceline-all-the-icons spaceline powerline all-the-icons doom-themes web-mode elixir-mode multi-term eterm-256color exec-path-from-shell neotree treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs evil general which-key use-package))
 '(pdf-view-midnight-colors (cons "#212337" "#e4f3fa"))
 '(rustic-ansi-faces
   ["#212337" "#ff757f" "#c3e88d" "#ffc777" "#82aaff" "#c099ff" "#b4f9f8" "#c8d3f5"])
 '(shell-pop-full-span t)
 '(shell-pop-window-size 30)
 '(vc-annotate-background "#212337")
 '(vc-annotate-color-map
   (list
    (cons 20 "#c7f59b")
    (cons 40 "#d9e28e")
    (cons 60 "#eccf82")
    (cons 80 "#ffbd76")
    (cons 100 "#ffb071")
    (cons 120 "#ffa36c")
    (cons 140 "#ff9668")
    (cons 160 "#e89d9a")
    (cons 180 "#d1a4cc")
    (cons 200 "#baacff")
    (cons 220 "#d18ecf")
    (cons 240 "#e8709f")
    (cons 260 "#ff5370")
    (cons 280 "#cd4d6b")
    (cons 300 "#9b4866")
    (cons 320 "#694361")
    (cons 340 "#383e5c")
    (cons 360 "#383e5c")))
 '(vc-annotate-very-old-color nil))
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
