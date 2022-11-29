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
  (global-lsp-bridge-mode)
  (setq acm-enable-icon t)
  )

(use-package clojure-mode
  )

(provide 'init-lsp-bridge)
