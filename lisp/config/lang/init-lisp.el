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
  :ensure t
  )

(use-package pretty-lambdada
  :ensure nil
  :hook
  (pretty-lambda . emacs-lisp-mode))

(global-leader 'emacs-lisp-mode-map
    "m" 'macrostep-expand
    "e" '(:ignore t :wk "eval")
    "eb" 'eval-buffer
    "ed" 'eval-defun
    "ee" 'eval-last-sexp
    "er" 'eval-region
    "el" 'load-library
    "g" '(:ignore t :wk "goto")
    "gf" 'find-function
    "gv" 'find-variable
    "gl" 'find-library)

(provide 'init-lisp)
;;; init-lang-elisp.el ends here
