;;; rust.el --- Summary
;;; Commentary:
;;; Code:
(require 'rust-funcs)
(require 'rust-keybinds)

(use-package rust-mode)
;;(use-package cargo-mode
;;  :hook
;;  ('rust-mode-hook . 'cargo-minor-mode))

;;(use-package flycheck-rust
;;  :hook
;;  ('flycheck-mode-hook . #'flycheck-rust-setup))

(setq racer-cmd "~/.cargo/bin/racer"
      racer-rust-src-path (concat (s-chop-suffix "\n" (shell-command-to-string "rustc --print sysroot")) "/lib/rustlib/src/rust/src"))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(provide 'rust)
;;; rust.el ends here
