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

(defun local/lsp-bridge-get-single-lang-server-by-project (project-path filepath)
  (let* ((json-object-type 'plist)
		 (custom-dir (expand-file-name ".cache/lsp-bridge/pyright" user-emacs-directory))
		 (custom-config (expand-file-name "pyright.json" custom-dir))
		 (default-config (json-read-file (expand-file-name "lisp/extensions/lsp-bridge/langserver/pyright.json" user-emacs-directory)))
		 (settings (plist-get default-config :settings)))
	(plist-put settings :pythonPath (executable-find "python3"))
	(make-directory (file-name-directory custom-config) t)
	(with-temp-file custom-config
	  (insert (json-encode default-config)))
	custom-config))

(defun +desmond/acm-terminal-require ()
  "Enable the acm terminal."
  (if (featurep 'acm-terminal)
      (+desmond/lsp-bridge-terminal-enable)
    (require 'acm-terminal)))

(defun +desmond/acm-enable ()
  "Request terminal environment package."
  (if (display-graphic-p)
      (+desmond/lsp-bridge-gui-enable)
    (+desmond/acm-terminal-require)))

(defun +desmond/lsp-bridge-gui-enable ()
  "Disable acm-terminal And remove advice."
  (interactive)
  (with-eval-after-load 'acm-terminal
    (advice-remove 'acm-frame-init-colors #'acm-terminal-init-colors)
    (advice-remove 'acm-hide #'acm-terminal-hide)
    (advice-remove 'acm-update #'acm-terminal-update)
    (advice-remove 'acm-doc-try-show  #'acm-terminal-doc-try-show)
    (advice-remove 'acm-doc-hide #'acm-terminal-doc-hide)
    (advice-remove 'acm-doc-scroll-up  #'acm-terminal-doc-scroll-up)
    (advice-remove 'acm-doc-scroll-down  #'acm-terminal-doc-scroll-down)
    (advice-remove 'acm-menu-max-length #'acm-terminal-max-length)
    (advice-remove 'acm-menu-render  #'acm-terminal-menu-render)
    (advice-remove 'acm-menu-render-items  #'acm-terminal-menu-render-items)
    (advice-remove 'acm-markdown-render-content  #'acm-terminal-markdown-render-content)))

(defun +desmond/lsp-bridge-terminal-enable ()
  "Enable acm-terminal ."
  (interactive)
  (with-eval-after-load 'acm-terminal
    (advice-add 'acm-frame-init-colors :override 'acm-terminal-init-colors)
    (advice-add 'acm-hide :override #'acm-terminal-hide)
    (advice-add 'acm-update :override #'acm-terminal-update)
    (advice-add 'acm-doc-try-show :override #'acm-terminal-doc-try-show)
    (advice-add 'acm-doc-hide :override #'acm-terminal-doc-hide)
    (advice-add 'acm-doc-scroll-up :override #'acm-terminal-doc-scroll-up)
    (advice-add 'acm-doc-scroll-down :override #'acm-terminal-doc-scroll-down)
    (advice-add 'acm-menu-max-length :filter-return #'acm-terminal-max-length)
    (advice-add 'acm-menu-render :override #'acm-terminal-menu-render)
    (advice-add 'acm-menu-render-items :override #'acm-terminal-menu-render-items)
    (advice-add 'acm-markdown-render-content :around #'acm-terminal-markdown-render-content)))

(use-package lsp-bridge
  :hook
  ((java-ts-mode java-mode) . lsp-bridge-mode)
  ((python-ts-mode python-mode) . lsp-bridge-mode)
  (web-mode . lsp-bridge-mode)
  ((typescript-mode typescript-ts-mode tsx-ts-mode) . lsp-bridge-mode)
  ((js-ts-mode) . lsp-bridge-mode)
  ;; 启用 lsp-bridge 时候 关闭 corfu
  (lsp-bridge-mode . (lambda () (corfu-mode -1)))
  ;; (python-ts-mode . lsp-bridge-mode)
  :init
  (require 'lsp-bridge-jdtls) ;; 根据项目自动生成自定义配置，添加必要的启动参数
  ;; 通过daemon启动时候也能正确加载terminal相关包
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'+desmond/acm-enable)
    (+desmond/acm-enable))
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
  ;; 全局启用 与 org-roam 有点冲突
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
  ;; (setq lsp-bridge-python-lsp-server "pyright")
  (setq lsp-bridge-python-multi-lsp-server "pyright_ruff")
  ;; python 配置 venv
  ;; (add-hook 'python-mode-hook (lambda () (setq-local lsp-bridge-get-single-lang-server-by-project 'local/lsp-bridge-get-single-lang-server-by-project)))
  ;; (add-hook 'python-ts-mode-hook (lambda () (setq-local lsp-bridge-get-single-lang-server-by-project 'local/lsp-bridge-get-single-lang-server-by-project)))
  ;; (add-hook 'pyvenv-post-activate-hooks (lambda () (lsp-bridge-restart-process)))
  ;; user lsp-config
  (setq lsp-bridge-user-langserver-dir (expand-file-name "lsp/langserver" user-emacs-directory))
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
(use-package eglot
  :hook
  (clojure-mode . eglot-ensure)
  (clojurescript-mode . eglot-ensure)
  :config
  (setq eglot-connect-timeout 60)
  (setq eglot-autoshutdown t)
  (setq eglot-send-changes-idle-time 0.2)
  (add-to-list 'eglot-server-programs '(clojure-mode "clojure-lsp"))
  (add-to-list 'eglot-server-programs '(clojurescript-mode "clojure-lsp"))
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  :general
  (:keymaps 'eglot-mode-map :states 'normal
  "gR" 'eglot-rename
  "gr" 'xref-find-references
  "gi" 'eglot-find-implementation
  "gh" 'eldoc
  "ga" 'eglot-code-actions))

;; (use-package eldoc-box
;;   :ensure t
;;   :config
;;   (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

(provide 'init-lsp)
