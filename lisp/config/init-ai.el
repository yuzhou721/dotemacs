;; -*- lexical-binding: t -*-
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
          :stream t
          :host "api.moonshot.cn")))

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

(provide 'init-ai)
;;; init-ai.el ends here.
