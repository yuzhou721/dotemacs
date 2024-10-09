;; -*- lexical-binding: t -*-
(use-package llm
  :ensure t)

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
  :config (setq khoj-api-key "kk-wb91yhp8364m9luzHg62o1bsKuvhQEmuVt-hUADYnZA"
                khoj-index-directories (list org-roam-directory)
                khoj-index-files (list +org-capture-todo-file
                                       +org-capture-inbox-file)))

(provide 'init-ai)
;;; init-ai.el ends here.
