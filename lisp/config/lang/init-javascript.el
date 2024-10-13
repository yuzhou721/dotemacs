;;; init-javascript --- js 初始化 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package js-mode
  :ensure nil
  :mode "\\.jsx\\'"
  :mode "\\.js\\'"
  :init
  (setq-default js-indent-level 2)
  (setq-local tab-width 2))

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
		js2-idle-timer-delay 0.15
        tab-width 2))

;; (use-package rjsx-mode
;;   :ensure nil
;;   :mode "\\.[mc]?js\\'"
;;   :mode "\\.es6\\'"
;;   :mode "\\.pac\\'"
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

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'"
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :ensure t
  :mode "\\.vue\\'"
  :mode "\\.wxml\\'"
  :init
  (setq web-mode-content-types-alist '(("vue" . "\\.vue\\'"))
		web-mode-css-indent-offset 2 ;; CSS 默认缩进 2 空格：包含 HTML 的 CSS 部分以及纯 CSS/LESS/SASS 文件等
		web-mode-code-indent-offset 2 ;; JavaScript 默认缩进 2 空格：包含 HTML 的 SCRIPT 部分以及纯 JS/JSX/TS/TSX 文件等
		web-mode-markup-indent-offset 2 ;; HTML 默认缩进 2 空格：包含 HTML 文件以及 Vue 文件的 TEMPLATE 部分
		web-mode-enable-css-colorization t ;; 开启 CSS 部分色值的展示：展示的时候会有光标显示位置异常
		web-mode-enable-auto-indentation nil ;; 禁止粘贴时格式化代码
		web-mode-enable-current-column-highlight nil)
	:config
	(setq tab-width 2))

(provide 'init-javascript)

