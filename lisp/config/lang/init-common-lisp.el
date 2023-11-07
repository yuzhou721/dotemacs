;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (use-package slime
;;   :ensure t
;;   :config
;;   (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (setq inferior-lisp-program "sbcl"))

(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

(provide 'init-common-lisp)
