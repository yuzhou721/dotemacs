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
  :config
  (corfu-mode -1)
  ;; lombok support
  (setq lombok-path (expand-file-name "plugins/lombok/lombok.jar" user-emacs-directory))
  (setq jvm-lombok-args (format "%s%s" "-javaagent:" lombok-path))
  (setq lsp-bridge-jdtls-jvm-args (list jvm-lombok-args))
  ;; evil
  (setq-local evil-goto-definition-functions '(lsp-bridge-jump))
  ;; 全局启用
  (global-lsp-bridge-mode)
  (setq acm-enable-icon t)
  ;; evil使用 lsp-bridge-jump
  ;; (evil-add-command-properties #'lsp-bridge-jump)
  (require 'lsp-bridge-jdtls) ;; 根据项目自动生成自定义配置，添加必要的启动参数
  (setq lsp-bridge-enable-auto-import t) ;; 开启自动导入依赖，目前没有code action。补全时可以通过这个导入相应的依赖，建议开启。
  (global-leader 'lsp-bridge-mode-map
      "a" 'lsp-bridge-code-action
      "d" 'lsp-bridge-find-define
      "p" 'lsp-bridge-peek)
  :bind
  (:map lsp-bridge-mode-map
	("s-j" . lsp-bridge-popup-documentation-scroll-down)
	("s-k" . lsp-bridge-popup-documentation-scroll-up)
	:map evil-normal-state-map
	("gi" . lsp-bridge-find-impl)
	("gh" . lsp-bridge-popup-documentation)
	("gn" . lsp-bridge-diagnostic-jump-next)
	("gp" . lsp-bridge-diagnostic-jump-prev)
	("ga" . lsp-bridge-code-action)
	("ge" . lsp-bridge-diagnostic-list)
	:map evil-motion-state-map
	("gR" . lsp-bridge-rename)
	("gr" . lsp-bridge-find-references)
	("gd" . lsp-bridge-jump)
	("gs" . lsp-bridge-restart-process)
	:map acm-mode-map
	("C-j" . acm-select-next)
	("C-k" . acm-select-prev))
)

;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
(defun lsp-bridge-jump ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (evil-goto-definition))
   ((eq major-mode 'org-mode)
    (org-agenda-open-link))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))
;; (use-package eglot
;;   :bind
;;   (:map eglot-mode-map
;;   ("gR" . #'eglot-rename)
;;   ("gr" . #'xref-find-references)
;;   ("gi" . #'eglot-find-implementation)
;;   ("gh" . #'eldoc)
;;   ("ga" . #'eglot-code-actions)))

(provide 'init-lsp)
