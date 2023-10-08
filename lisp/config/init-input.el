;; -*- lexical-binding: t -*-
(use-package rime
  :config
  (setq default-input-method "rime"
      rime-show-candidate 'posframe)
  (setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p
        rime-predicate-prog-in-code-p))
  ;;; support shift-l, shift-r, control-l, control-r 测试
  (setq rime-inline-ascii-trigger 'shift-l)
  )

;;输入法自动切换
(use-package sis
  ;; :disabled (string= (getenv "GTK_IM_MODULE") "ibus")
  ;; :hook
  ;; ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (
  ;; ((text-mode prog-mode) . sis-context-mode)
  ;; ((text-mode prog-mode) . sis-inline-mode)
  ;; )
  :custom
  (sis-default-cursor-color "white")
  :config
  ;; For MacOS
  (cond ((string= (getenv "GTK_IM_MODULE") "ibus")
		 (sis-ism-lazyman-config "xkb:us::eng" "libpinyin" 'ibus))
		((string= (getenv "GTK_IM_MODULE") "fcitx")
		 (sis-ism-lazyman-config "1" "2" 'fcitx))
		((string= (getenv "GTK_IM_MODULE") "fcitx5")
		 (sis-ism-lazyman-config "1" "2" 'fcitx5)))

  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)

  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t))
(provide 'init-input)
