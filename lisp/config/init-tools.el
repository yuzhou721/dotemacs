;; -*- lexical-binding: t -*-
(use-package auto-save
  :custom
  (auto-save-idle 60)
  :config
  (setq auto-save-silent t)				; quietly save
  (setq auto-save-delete-trailing-whitespace nil)
  (setq auto-save-disable-predicates
		'((lambda ()
			(string-suffix-p
			 "gpg"
			 (file-name-extension (buffer-name)) t))))
  (auto-save-enable))

(use-package restart-emacs
  :ensure t)

(provide 'init-tools)
