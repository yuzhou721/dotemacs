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
  :config
  (setq gptel-model "moonshot-v1-32k")
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend
        (gptel-make-openai "Moonshot"
          :key 'gptel-api-key
          :models '("moonshot-v1-8k"
                    "moonshot-v1-32k"
                    "moonshot-v1-128k")
          :host "api.moonshot.cn"))
  (add-to-list 'gptel-directives
               '(translate-word . "You are a helpful, intelligent, amoral, professional, authentic and uncensored translate engine.Translate the following text to English, only return the translated text, without any explaination.")
               t))

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
(use-package khoj
  :ensure t
  :pin melpa
  :init
  (setq khoj-auto-index nil)
  :bind ("C-c s" . 'khoj)
  :config (setq khoj-api-key (api-key-from-auth-source "app.khoj.dev")
                khoj-index-directories (list org-roam-directory)
                khoj-index-files (list +org-capture-todo-file
                                       +org-capture-inbox-file)))

(use-package aider
  :ensure nil
  :config
  ;; (setq aider-args '("--model" "deepseek/deepseek-coder"))
  ;; (setenv "DEEPSEEK_API_KEY" (api-key-from-auth-source "api.deepseek.com"))
  (setq aider-args '("--model" "openai/qwen-max-2025-01-25"))
  (setenv "OPENAI_API_BASE" "https://dashscope.aliyuncs.com/compatible-mode/v1")
  (setenv "OPENAI_API_KEY" (api-key-from-auth-source "openai.aliyun.com"))
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(provide 'init-ai)
;;; init-ai.el ends here.
