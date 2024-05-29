;; -*- lexical-binding: t -*-
;;; Code: Highlight TODO
(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode))

;; Show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))

;; Lint tool
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe))

;; Config files mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Syntax highlighting for systemd files
(use-package conf-mode
  :ensure nil
  :mode ((rx "."
             (or "automount" "busname" "link" "mount" "netdev" "network"
                 "path" "service" "slice" "socket" "swap" "target" "timer")
             string-end) . conf-toml-mode))

(use-package rainbow-delimiters
  :ensure nil
  :hook (prog-mode . rainbow-delimiters-mode))

;; 类 lisp 语言结构编辑
(use-package lispy
  :ensure nil
  :hook
  (emacs-lisp-mode . lispy-mode)
  (clojure-mode . lispy-mode)
  (lisp-mode . lispy-mode)
  :init
  :config
  (lispy-define-key lispy-mode-map "e" 'eval-last-sexp))

(use-package lispyville
  :ensure nil
  :after lispy
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w additional prettify)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package dape
  :config
  (add-to-list 'dape-configs
			   `(debugpy
				 modes (python-ts-mode python-mode)
				 command "python3"
				 command-args ("-m" "debugpy.adapter")
				 :type "executable"
				 :request "launch"
				 :cwd dape-cwd-fn
				 :program dape-find-file-buffer-default)))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'clojure-mode "'" nil :actions nil)
  (sp-local-pair 'clojure-mode "`" nil :actions nil)
  (sp-local-pair 'clojurescript-mode "'" nil :actions nil)
  (sp-local-pair 'clojurescript-mode "`" nil :actions nil)
  (sp-local-pair 'cider-repl-mode "'" nil :actions nil)
  (sp-local-pair 'cider-repl-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "`" nil :actions nil)
  (sp-local-pair 'sly-mrepl-mode "'" nil :actions nil)
  (sp-local-pair 'sly-mrepl-mode "`" nil :actions nil)
  :config
    (sp-with-modes
        '(c++-mode objc-mode c-mode javascript-mode)
      (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package prettier
  :ensure t
  :config
  (setq prettier-pre-warm 'some))

(use-package insert-translated-name
  :config
  ;; Set translation engine
  (setq insert-translated-name-program "llm")
  (require 'llm-openai)
  (setq llm-warn-on-nonfree nil)
  (setq insert-translated-name-llm-provider (make-llm-openai-compatible
                                             :key (insert-translated-name-api-key-from-auth-source "api.deepseek.com")
                                             :chat-model "deepseek-chat"
                                             :url "https://api.deepseek.com/v1")))
(use-package diff-hl
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(require 'init-lisp)
(require 'init-javascript)
(require 'init-tree-sitter)
(require 'init-dart)
(require 'init-clojure)
(require 'init-common-lisp)
(require 'init-golang)
(provide 'init-dev)
