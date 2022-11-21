;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package tree-sitter
  :ensure nil
  )
(use-package tree-sitter-hl
  :ensure nil
  )
(use-package tree-sitter-langs
  :ensure nil
  )
(use-package tree-sitter-debug
  :ensure nil
  )
(use-package tree-sitter-query
  :ensure nil
  )

(use-package grammatical-edit
  :ensure nil
  :config
  (dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               'typescript-mode-hook
               ))
  (add-hook hook '(lambda () (grammatical-edit-mode 1))))
  )

(provide 'init-tree-sitter)
