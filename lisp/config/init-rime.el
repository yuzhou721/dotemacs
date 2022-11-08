;; -*- lexical-binding: t -*-
(use-package rime
  :config
  (setq default-input-method "rime"
      rime-show-candidate 'posframe)
  (setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p
        rime-predicate-prog-in-code-p))
  )

(provide 'init-rime)
