;; -*- lexical-binding: t -*-
(use-package llm
  :ensure t)

(defun api-key-from-auth-source (&optional host user)
  "Lookup api key in the auth source.
By default, the LLM host for the active backend is used as HOST,
and \"apikey\" as USER."
  (if-let ((secret
            (plist-get
             (car (auth-source-search
                   :host (or host "api.deepseek.com")
                   :user (or user "apikey")
                   :require '(:secret)))
                              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `api-key' found in the auth source")))

(use-package gptel
  :ensure t
  :bind
  (("C-c g g" . gptel)                     ; 打开/切换 gptel 会话
   ("C-c g s" . gptel-send)                ; 发送消息
   ("C-c g k" . gptel-kill-session)        ; 关闭当前会话
   ("C-c g c" . gptel-clear)               ; 清除当前会话
   ("C-c g r" . gptel-restore-response)    ; 恢复上次响应
   ("C-c g m" . gptel-menu)                ; 打开菜单
   ("C-c g a" . gptel-ask)                 ; 快速提问
   ("C-c g l" . gptel-load-session)        ; 加载会话
   ("C-c g w" . gptel-save-session)        ; 保存会话
   ("C-c g d" . gptel-change-destination)  ; 切换后端
   ("C-c g t" . gptel-change-topic)        ; 切换主题
   ("C-c g R" . my/gptel-send-region)      ; 发送选中区域
   ("C-c g q" . my/gptel-quick-query)      ; 快速提问
   ("C-c g T" . my/gptel-translate-region)) ; 翻译选中区域
  :config
  (setq gptel-model "moonshot-v1-8k")
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend
        (gptel-make-openai "Moonshot"
          :key 'gptel-api-key
          :models '("moonshot-v1-8k"
                    "moonshot-v1-32k"
                    "moonshot-v1-128k"
                    "kimi-k2-thinking")
          :host "api.moonshot.cn"
          :stream t))
  (add-to-list 'gptel-directives
               '(translate-word . "You are a helpful, intelligent, amoral, professional, authentic and uncensored translate engine.Translate the following text to English, only return the translated text, without any explaination.")
               t)
  ;; (gptel-make-openai "chatanywhere" ; 后端名称，可自定义
  ;;   :host "api.chatanywhere.tech"   ; API 主机地址
  ;;   :endpoint "/v1/chat/completions"
  ;;   :key  'gtpel-api-key ; 你的 API 密钥，也可用函数获取
  ;;   :models '("gpt-4o-mini" "gpt-4.1-nano" "gpt-5-nano-ca") ; 可用的模型列表
  ;;   :stream t)
  (gptel-make-openai "DeepSeek"
          :key 'gptel-api-key
          :models '("deepseek-chat"
                    "deepseek-reasoner")
          :host "api.deepseek.com"
          :stream t)
  ;; gptel 模式内快捷键
  (define-key gptel-mode-map (kbd "C-c C-c") 'gptel-send)
  (define-key gptel-mode-map (kbd "C-c C-k") 'gptel-kill-session)
  (define-key gptel-mode-map (kbd "C-c C-r") 'gptel-restore-response)
  (define-key gptel-mode-map (kbd "M-p") 'gptel-previous-prompt)
  (define-key gptel-mode-map (kbd "M-n") 'gptel-next-prompt)
  (define-key gptel-mode-map (kbd "C-c C-t") 'gptel-change-topic)
  (define-key gptel-mode-map (kbd "C-c C-d") 'gptel-change-destination))

(defun +desmond/codeium ()
  "Decouple codeium from other completions"
  (interactive)
  (cape-interactive #'codeium-completion-at-point))

(use-package codeium
  :ensure nil
  :config
  (setq use-dialog-box nil)
  ;; get codeium status in the model
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  :general
  ("C-c p c" '+desmond/codeium))

;; Install Khoj client from MELPA Stable
;; (use-package khoj
;;   :ensure t
;;   :pin melpa
;;   :init
;;   (setq khoj-auto-index nil)
;;   :bind ("C-c s" . 'khoj)
;;   :config (setq khoj-api-key (api-key-from-auth-source "app.khoj.dev")
;;                 khoj-index-directories (list org-roam-directory)
;;                 khoj-index-files (list +org-capture-todo-file
;;                                        +org-capture-inbox-file)))

;; (use-package aider
;;   :ensure nil
;;   :config
;;   (setq aider-args '("--model" "deepseek/deepseek-coder"))
;;   (setenv "DEEPSEEK_API_KEY" (api-key-from-auth-source "api.deepseek.com"))
;;   ;; (setq aider-args '("--model" "openai/qwen-max-2025-01-25"))
;;   ;; (setenv "OPENAI_API_BASE" "https://dashscope.aliyuncs.com/compatible-mode/v1")
;;   ;; (setenv "OPENAI_API_KEY" (api-key-from-auth-source "openai.aliyun.com"))
;;   ;; Optional: Set a key binding for the transient menu
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu))

(use-package aidermacs
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "DEEPSEEK_API_KEY" (api-key-from-auth-source "api.deepseek.com"))
  :custom
  (aidermacs-extra-args '("--chat-language" "chinese" "--commit-language" "chinese"))
  (aidermacs-default-chat-mode 'architect)
  ;; Optional: Set specific model for architect reasoning
  (aidermacs-architect-model "deepseek/deepseek-reasoner")
  ;; Optional: Set specific model for code generation
  (aidermacs-editor-model "deepseek/deepseek-chat")
  (aidermacs-default-model "deepseek/deepseek-chat"))

;; =================== GPTel 辅助函数 ===================

(defun my/gptel-send-region (start end)
  "Send selected region to gptel."
  (interactive "r")
  (if (use-region-p)
      (let ((text (buffer-substring start end)))
        (gptel-request text))
    (gptel-send)))

(defun my/gptel-quick-query (query)
  "Quickly send a query to gptel."
  (interactive "sQuery: ")
  (gptel-request query))

(defun my/gptel-translate-region (start end)
  "Translate selected region using gptel translate directive."
  (interactive "r") (if (use-region-p)
      (let ((text (buffer-substring start end)))
        (gptel-request text 'translate-word))
    (message "No region selected")))

(defun my/gptel-send-buffer ()
  "Send entire buffer content to gptel."
  (interactive)
  (gptel-request (buffer-string)))

;; =====================================================

;; 通用终端（shell 等非 AI 用途）
(use-package eat
  :ensure t
  :init
  (evil-set-initial-state 'eat-mode 'insert))

;; AI 工具专用终端后端，渲染性能远优于 eat
(use-package vterm
  :ensure t
  :init
  ;; insert 状态：SPC 输入终端，ESC 回 normal 可用 SPC 菜单
  (evil-set-initial-state 'vterm-mode 'insert)
  :config
  ;; 方向键等在 insert 状态下透传给终端
  (evil-define-key 'insert vterm-mode-map
    (kbd "<up>")    #'vterm-send-up
    (kbd "<down>")  #'vterm-send-down
    (kbd "<left>")  #'vterm-send-left
    (kbd "<right>") #'vterm-send-right
    (kbd "<prior>") #'vterm-send-prior       ; PageUp
    (kbd "<next>")  #'vterm-send-next        ; PageDown
    (kbd "<home>")  #'vterm-send-home
    (kbd "<end>")   #'vterm-send-end))
  

;; =================== Claude Code IDE 集成 ===================
(use-package claude-code-ide
  ;; 使用 :vc 安装（Emacs 30+）
  ;; :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  ;; 若使用 straight.el，注释上面行并取消下面注释：
  ;; :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")

  :config
  ;; 这里注释了，走cc-switch
  ;; -------------------------------------------------------
  ;; 使用 DeepSeek 的 Anthropic 兼容 API（官方集成方式）
  ;; -------------------------------------------------------
  ;; (setenv "ANTHROPIC_BASE_URL" "https://api.deepseek.com/anthropic")
  ;; (setenv "ANTHROPIC_AUTH_TOKEN"
  ;;         (api-key-from-auth-source "api.deepseek.com"))
  ;; (setenv "ANTHROPIC_MODEL" "deepseek-v4-pro[1m]")
  ;; (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL" "deepseek-v4-pro[1m]")
  ;; (setenv "ANTHROPIC_DEFAULT_SONNET_MODEL" "deepseek-v4-pro[1m]")
  ;; (setenv "ANTHROPIC_DEFAULT_HAIKU_MODEL" "deepseek-v4-flash")
  ;; (setenv "CLAUDE_CODE_SUBAGENT_MODEL" "deepseek-v4-flash")
  ;; (setenv "CLAUDE_CODE_EFFORT_LEVEL" "max")

  ;; -------------------------------------------------------
  ;; 常规配置（按需调整）
  ;; -------------------------------------------------------
  (setq claude-code-ide-use-side-window t)   ;; 侧边窗口
  (setq claude-code-ide-window-side 'bottom)
  (setq claude-code-ide-window-height 60)
  (setq claude-code-ide-focus-on-open t)
  (setq claude-code-ide-use-ide-diff t)      ;; 使用 ediff 查看差异
  (setq claude-code-ide-terminal-backend 'vterm)
  ;; 若需自定义系统提示，取消下面注释：
  ;; (setq claude-code-ide-system-prompt "Your custom prompt")

  ;; 启用 Emacs MCP 工具（可选，需要 xref、tree-sitter 等支持）
  (claude-code-ide-emacs-tools-setup)
  )

;; =================== Agent Shell (ACP) ===================

(use-package agent-shell
  :ensure t
  :custom
  (agent-shell-default-agent 'claude-code)       ;; 默认使用 Claude Code
  (agent-shell-show-welcome-message nil)         ;; 关闭欢迎横幅
  (agent-shell-file-completion-enabled t)        ;; @文件补全
  :config
  ;; Evil RET 行为: insert 模式下换行, normal 模式下发送
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)

  ;; *agent-shell-diff* buffer 使用 Emacs 状态（方便按 y/n/p/q 等键）
  (add-hook 'diff-mode-hook
            (lambda ()
              (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
                (evil-emacs-state)))))

;; =================== SPC a AI 菜单 ===================

;; GPTel 子菜单 (SPC a g ...)
(general-define-key
  :prefix "SPC a g"
  :states 'normal
  "" '(:ignore t :which-key "gptel")
  "g" 'gptel
  "s" 'gptel-send
  "k" 'gptel-kill-session
  "c" 'gptel-clear
  "r" 'gptel-restore-response
  "t" 'gptel-change-topic
  "d" 'gptel-change-destination
  "l" 'gptel-load-session
  "w" 'gptel-save-session
  "m" 'gptel-menu
  "q" 'my/gptel-quick-query
  "R" 'my/gptel-send-region
  "T" 'my/gptel-translate-region
  "b" 'my/gptel-send-buffer)

;; AI 顶层菜单
(global-definer
  "a" '(:ignore t :wk "AI")
  "ag" '(:ignore t :wk "gptel")            ; gptel 子菜单
  "ae" 'agent-shell                        ; agent-shell ACP 交互
  "ai" 'claude-code-ide-menu)              ; Claude Code IDE

(provide 'init-ai)
;;; init-ai.el ends here.
