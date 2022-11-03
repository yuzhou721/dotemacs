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
