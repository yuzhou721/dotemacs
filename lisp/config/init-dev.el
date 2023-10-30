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
  :hook
  (emacs-lisp-mode . lispy-mode)
  (clojure-mode . lispy-mode)
  :init
  :config
  (lispy-define-key lispy-mode-map "e" 'eval-last-sexp))

(use-package lispyville
  :after lispy
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w additional prettify)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(require 'init-lisp)
(require 'init-javascript)
(require 'init-tree-sitter)
(require 'init-dart)
(require 'init-clojure)
(provide 'init-dev)
