;;; init-javascript --- js 初始化 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package js-mode
  :ensure nil
  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"
  )

(use-package js2-mode
  :ensure nil
  :hook (js2-minor-mode . js-mode)
  :config
   (setq js-chain-indent t
        ;; These have become standard in the JS community
        js2-basic-offset 2
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-idle-timer-delay 0.15)
  )

;; (use-package rjsx-mode
;;   :ensure nil
;;   :requires js2-mode
;;   :interpreter "node"
;;   :hook (rjsx-mode . js2-minor-mode)
;;   :init
;;   ;; Parse node stack traces in the compilation buffer
;;   ;; (with-eval-after-load compilation
;;   ;;   (add-to-list 'compilation-error-regexp-alist 'node)
;;   ;;   (add-to-list 'compilation-error-regexp-alist-alist
;;   ;;                '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
;;   ;;                       2 3 4)))
;;   )

(use-package typescript-mode
  :ensure nil
  :hook (typescript-mode . rainbow-delimiters-mode)
  :hook (typescript-tsx-mode . rainbow-delimiters-mode)
  )

(use-package web-mode
  :ensure nil
  )

(provide 'init-javascript)

