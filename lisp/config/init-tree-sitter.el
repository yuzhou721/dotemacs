;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package treesit
  :if (and (treesit-available-p)
     (>= emacs-major-version 29))
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install 'prompt))

(use-package combobulate
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (css-ts-mode . combobulate-mode)
           (yaml-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode)
           (java-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode))
    )

(use-package fingertip
  :hook
  (java-ts-mode . fingertip-mode)
  :general
  (:keymaps 'fingertip-mode :states '(insert visual) :definer 'minor-mode
	    "(" 'fingertip-open-round
	    "(" 'fingertip-open-round
	    "[" 'fingertip-open-bracket
	    "{" 'fingertip-open-curly
	    ")" 'fingertip-close-round
	    "]" 'fingertip-close-bracket
	    "}" 'fingertip-close-curly
	    "=" 'fingertip-equal
	    "（" 'fingertip-open-chinese-round
	    "「" 'fingertip-open-chinese-bracket
	    "【" 'fingertip-open-chinese-curly
	    "）" 'fingertip-close-chinese-round
	    "」" 'fingertip-close-chinese-bracket
	    "】" 'fingertip-close-chinese-curly
	    "\"" 'fingertip-double-quote
	    "'"  'fingertip-single-quote))

(use-package evil-textobj-tree-sitter
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  )
(provide 'init-tree-sitter)
