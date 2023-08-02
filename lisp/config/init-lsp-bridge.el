;; -*- lexical-binding: t -*-
;; 弹窗
(use-package posframe
  :ensure t)

(use-package dumb-jump
  :ensure nil
  :config
  (setq dumb-jump-selector 'completion-read)
  )

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
  (setq lombok-path (expand-file-name "plugins/lombok/lombok.jar" user-emacs-directory))
  (setq jvm-lombok-args (format "%s%s" "-javaagent:" lombok-path))
  (setq lsp-bridge-jdtls-jvm-args (list jvm-lombok-args))
  ;; evil
  (setq-local evil-goto-definition-functions '(lsp-bridge-jump))
  ;; 全局启用
  (global-lsp-bridge-mode)
  (setq acm-enable-icon t)
  ;; evil使用lsp-bridge-jump
  (evil-add-command-properties #'lsp-bridge-jump)
  :bind
  (:map evil-motion-state-map
	("gR" . lsp-bridge-rename)
	("gr" . lsp-bridge-find-references)
	("gd" . lsp-bridge-jump)
	("gs" . lsp-bridge-restart-process)
	:map evil-normal-state-map
	("gi" . lsp-bridge-find-impl)
	("gh" . lsp-bridge-popup-documentation)
	("gn" . lsp-bridge-diagnostic-jump-next)
	("gp" . lsp-bridge-diagnostic-jump-prev)
	("ga" . lsp-bridge-code-action)
	("ge" . lsp-bridge-diagnostic-list)
	:map lsp-bridge-mode-map
	("s-j" . lsp-bridge-popup-documentation-scroll-down)
	("s-k" . lsp-bridge-popup-documentation-scroll-up)
	:map acm-mode-map
	("C-j" . acm-select-next)
	("C-k" . acm-select-prev))
  )

(defun lsp-bridge-jump()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (evil-goto-definition))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))


(provide 'init-lsp-bridge)
