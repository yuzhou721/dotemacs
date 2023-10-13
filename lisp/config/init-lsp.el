;; -*- lexical-binding: t -*-
;; 弹窗
(use-package posframe
  :ensure t)

(use-package dumb-jump
  :ensure nil
  :config
  (setq dumb-jump-selector 'completion-read)
  )

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package lsp-bridge
  :hook
  ((java-ts-mode java-mode) . lsp-bridge-mode)
  ((python-ts-mode python-mode) . lsp-bridge-mode)
  ;; 启用 lsp-bridge 时候 关闭 corfu
  (lsp-bridge-mode . (lambda () (corfu-mode -1)))
  ;; (python-ts-mode . lsp-bridge-mode)
  :init
  (require 'lsp-bridge-jdtls) ;; 根据项目自动生成自定义配置，添加必要的启动参数
  :config
  ;; Output server logs to `*lsp-bridge*' buffer, required restarting the process
  (setq lsp-bridge-enable-log nil)
  (setq lsp-bridge-enable-debug nil)
  ;; Show tooltip when cursor under a diagnostic overlay
  (setq lsp-bridge-enable-hover-diagnostic t)
  ;; Do not display documentation by default, press `M-d' when needed
  (setq acm-enable-doc nil)
  ;; Evil initial state
  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)
  ;; evil
  ;; (setq-local evil-goto-definition-functions '(lsp-bridge-jump))
  ;; 全局启用
  ;; (global-lsp-bridge-mode)
  (setq acm-enable-icon t)
  ;; evil使用 lsp-bridge-jump
  ;; (evil-add-command-properties #'lsp-bridge-jump)
  ;; java 配置
  ;; lombok support
  (setq lombok-path (expand-file-name "plugins/lombok/lombok.jar" user-emacs-directory))
  (setq jvm-lombok-args (format "%s%s" "-javaagent:" lombok-path))
  (setq lsp-bridge-jdtls-jvm-args (list jvm-lombok-args))
  (setq lsp-bridge-enable-auto-import t) ;; 开启自动导入依赖，目前没有code action。补全时可以通过这个导入相应的依赖，建议开启。
  (setq lsp-bridge-enable-org-babel t)
  ;; python 配置 先安装pyright和ruff
  (setq lsp-bridge-python-command "python3")
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
  :general
  (:states 'normal :keymaps 'lsp-bridge-mode-map
	   "gi" 'lsp-bridge-find-impl
	   "gh" 'lsp-bridge-popup-documentation
	   "gn" 'lsp-bridge-diagnostic-jump-next
	   "gp" 'lsp-bridge-diagnostic-jump-prev
	   "ga" 'lsp-bridge-code-action
	   "ge" 'lsp-bridge-diagnostic-list)
  (:states 'motion :keymaps 'lsp-bridge-mode-map
	   "gR" 'lsp-bridge-rename
	   "gr" 'lsp-bridge-find-references
	   "gd" 'lsp-bridge-find-def)
  (:keymaps 'acm-mode-map
	    "C-j" 'acm-select-next
	    "C-k" 'acm-select-prev)
  (:keymaps 'lsp-bridge-mode-map
	    "S-j" 'lsp-bridge-popup-documentation-scroll-down
	    "S-k" 'lsp-bridge-popup-documentation-scroll-up)
  ;; 设置按键
  (global-leader 'lsp-bridge-mode-map
      "a" 'lsp-bridge-code-action
      "d" 'lsp-bridge-find-def
      "p" 'lsp-bridge-peek
      "r" 'lsp-bridge-restart-process))

;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
;; (defun lsp-bridge-jump ()
;;   (interactive)
;;   (cond
;;    ((eq major-mode 'emacs-lisp-mode)
;;     (evil-goto-definition))
;;    ((eq major-mode 'org-mode)
;;     (org-agenda-open-link))
;;    (lsp-bridge-mode
;;     (lsp-bridge-find-def))
;;    (t
;;     (require 'dumb-jump)
;;     (dumb-jump-go))))
;; (use-package eglot
;;   :bind
;;   (:map eglot-mode-map
;;   ("gR" . #'eglot-rename)
;;   ("gr" . #'xref-find-references)
;;   ("gi" . #'eglot-find-implementation)
;;   ("gh" . #'eldoc)
;;   ("ga" . #'eglot-code-actions)))

(provide 'init-lsp)
