;; -*- lexical-binding: t -*-
(use-package fanyi
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider))
  )


(use-package auto-save
  :custom
  (auto-save-idle 15)
  :config
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace nil)
  (setq auto-save-disable-predicates
      '((lambda ()
      (string-suffix-p
      "gpg"
      (file-name-extension (buffer-name)) t))))
  (auto-save-enable)
  )

(use-package restart-emacs
  :ensure t)

(provide 'init-tools)
