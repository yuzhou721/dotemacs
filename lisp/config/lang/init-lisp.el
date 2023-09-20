;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lispy
  :ensure t
  :hook (emacs-lisp-mode . lispy-mode)
  :init
  :config
  (lispy-define-key lispy-mode-map "e" 'eval-last-sexp)
  )

(use-package lispyville
  :ensure t
  :after lispy
  :hook (lispy-mode . lispyville-mode))

(use-package macrostep
  :ensure t)

(use-package pretty-lambdada
  :ensure nil
  :hook
  (pretty-lambda . emacs-lisp-mode))

(provide 'init-lisp)
;;; init-lang-elisp.el ends here
