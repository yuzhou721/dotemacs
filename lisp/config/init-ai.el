;; -*- lexical-binding: t -*-
(use-package gptel
  :ensure t
  :config
  (setq gptel-model "moonshot-v1-8k")
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend
        (gptel-make-openai "Moonshot"
          :key 'gptel-api-key
          :models '("moonshot-v1-8k"
                    "moonshot-v1-32k"
                    "moonshot-v1-128k")
          :host "api.moonshot.cn")))

(use-package ellama
  :ensure t
  :config
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "Chinese")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (require 'llm-openai)
  (setopt ellama-providers
          '(("code" . (make-llm-ollama
                       :chat-model "codegemma:code"
                       :embedding-model "nomic-embed-text"))
            ("llama2-chinese" .  (make-llm-ollama
                                  ;; this model should be pulled to use it
                                  ;; value should be the same as you print in terminal during pull
                                  :chat-model "llama2-chinese"
                                  :embedding-model "nomic-embed-text"))))
  ; Naming new sessions with llm
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "llama2-chinese"
           :embedding-model "nomic-embed-text"))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm))

(use-package codeium
  :ensure nil
  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :config
  (setq use-dialog-box nil)
  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion)))))

(provide 'init-ai)
;;; init-ai.el ends here.
