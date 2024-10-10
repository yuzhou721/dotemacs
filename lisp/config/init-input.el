;; -*- lexical-binding: t -*-
(use-package rime
  :demand t
  ;; :init
  ;; (setq default-input-method "rime")
  :config
  (setq rime-show-candidate 'posframe)
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-hydra-p
          ;; rime-predicate-in-code-string-p
          rime-predicate-prog-in-code-p))
;;; support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-l)
  :general
  (:keymaps 'rime-active-mode-map
            "<tab>" 'rime-inline-ascii)
  (:keymaps 'rime-mode-map
            "M-j" 'rime-force-enable
            "M-k" 'rime-inline-ascii))

(defun +desmond/sis-mode-set (status)
  "Sis mode set. STATUS is status code ."
  (with-no-warnings
    (sis-global-cursor-color-mode status)
    (sis-global-respect-mode status)
    (sis-global-context-mode status)
    (sis-global-inline-mode status)))

(defun +desmond/sis-clear-hook ()
  "Remove sis hook."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (dolist (hook sis-context-hooks)
        (remove-hook hook #'sis-context t)))))

(defun +desmond/sis-mode-swich ()
  "Sis swicher."
  (interactive)
  (with-no-warnings
    (if sis-global-context-mode
        (progn  (+desmond/sis-mode-set -1)
            (+desmond/sis-clear-hook))
      (+desmond/sis-mode-set t))))

;;输入法自动切换
(use-package sis
  :custom
  (sis-default-cursor-color "white")
  :config
  ;; For Linux“
  (cond ((string= (getenv "GTK_IM_MODULE") "ibus")
		 (sis-ism-lazyman-config "xkb:us::eng" "libpinyin" 'ibus))
		((string= (getenv "GTK_IM_MODULE") "fcitx")
		 (sis-ism-lazyman-config "1" "2" 'fcitx))
		((string= (getenv "GTK_IM_MODULE") "fcitx5")
		 (sis-ism-lazyman-config "1" "2" 'fcitx5))
        ((string= system-type "darwin")
         ;; Not needed if your input sources are the same with the default values
         (sis-ism-lazyman-config
          "com.apple.keylayout.US"
          "com.sogou.inputmethod.sogou.pinyin")))
  (+desmond/sis-mode-set t))
(provide 'init-input)
