;; -*- lexical-binding: t -*-
;; 弹窗
(use-package posframe
  :ensure t)

(use-package yasnippet
  :ensure nil
  :config
  (yas-global-mode 1)
  )

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package lsp-bridge
  :ensure nil
  :config
  ;; lombok support
  (setq lombok-path (substitute-in-file-name "$HOME/dotfiles/private/libraries/lombok.jar"))
  (setq lsp-bridge-jdtls-jvm-args (format "%s%s" "-javaagent:" lombok-path))
  (global-lsp-bridge-mode)
  (setq acm-enable-icon t)
  )

(provide 'init-lsp-bridge)
