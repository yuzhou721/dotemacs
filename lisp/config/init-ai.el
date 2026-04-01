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
          :stream t))

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
  (interactive "r")
  (if (use-region-p)
      (let ((text (buffer-substring start end)))
        (gptel-request text 'translate-word))
    (message "No region selected")))

(defun my/gptel-send-buffer ()
  "Send entire buffer content to gptel."
  (interactive)
  (gptel-request (buffer-string)))

;; 绑定到快捷键（可选）
(general-define-key
 :prefix "C-c g"
 "b" 'my/gptel-send-buffer)

;; =====================================================

(provide 'init-ai)
;;; init-ai.el ends here.
