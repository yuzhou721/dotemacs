;; -*- lexical-binding: t -*-
(use-package rime
  :demand t
  :init
  (setq default-input-method "rime")
  :config
  (if (display-graphic-p)
      (setq rime-show-candidate 'posframe)
    (setq rime-show-candidate 'minibuffer))
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-hydra-p
          ;; rime-predicate-in-code-string-p
          rime-predicate-prog-in-code-p))
;;; support shift-l, shift-r, control-l, control-r 测试
  (setq rime-inline-ascii-trigger 'shift-l)
  :general
  (:keymaps 'rime-active-mode-map
            "<tab>" 'rime-inline-ascii)
  (:keymaps 'rime-mode-map
            "M-j" 'rime-force-enable))

(defun +desmond/sis-mode-set (status)
  "sis mode set."
  (with-no-warnings
    (sis-global-cursor-color-mode status)
    ;; (sis-global-respect-mode status)
    (sis-global-context-mode status)
    (sis-global-inline-mode status)))

(defun +desmond/sis-mode-swich ()
  "sis swicher."
  (interactive)
  (with-no-warnings
    (if (sis-global-context-mode)
        (+desmond/sis-mode-set nil)
      (+desmond/sis-mode-set t))))
;;输入法自动切换
(use-package sis
  ;; :disabled (string= (getenv "GTK_IM_MODULE") "ibus")
  ;; :hook
  ;; ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (
  ;; ((text-mode prog-mode) . sis-context-mode)
  ;; ((text-mode prog-mode) . sis-inline-mode)
  ;; )
  ;; chrome os 里的输入发会冲突
  ;; :if (and (display-graphic-p)
  ;;         (not (string= "penguin" system-name)))
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
  (+desmond/sis-mode-set t))
(provide 'init-input)
