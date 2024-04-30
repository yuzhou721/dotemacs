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
  (treesit-auto-add-to-auto-mode-alist 'all)
  (defvar genehack/tsx-treesit-auto-recipe
    (make-treesit-auto-recipe
     :lang 'tsx
     :ts-mode 'tsx-ts-mode
     :remap '(typescript-tsx-mode)
     :requires 'typescript
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "v0.20.3"
     :source-dir "tsx/src"
     :ext "\\.tsx\\'")
    "Recipe for libtree-sitter-tsx.dylib")
  (add-to-list 'treesit-auto-recipe-list genehack/tsx-treesit-auto-recipe)
  (defvar genehack/typescript-treesit-auto-recipe
    (make-treesit-auto-recipe
     :lang 'typescript
     :ts-mode 'typescript-ts-mode
     :remap 'typescript-mode
     :requires 'tsx
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "v0.20.3"
     :source-dir "typescript/src"
     :ext "\\.ts\\'")
    "Recipe for libtree-sitter-typescript.dylib")
  (add-to-list 'treesit-auto-recipe-list genehack/typescript-treesit-auto-recipe)
  (defvar genehack/javascript-treesit-auto-recipe
    (make-treesit-auto-recipe
      :lang 'javascript
      :ts-mode 'js-ts-mode
      :remap '(js-mode javascript-mode js2-mode)
      :url "https://github.com/tree-sitter/tree-sitter-javascript"
      :revision "v0.20.1"
      :source-dir "src"
      :ext "\\.js\\'"))
  (add-to-list 'treesit-auto-recipe-list genehack/javascript-treesit-auto-recipe)
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
      :lang 'go
      :ts-mode 'go-ts-mode
      :remap 'go-mode
      :requires 'gomod
      :url "https://github.com/tree-sitter/tree-sitter-go"
      :revision "v0.20.0"
      :ext "\\.go\\'"))
  (setq treesit-auto-install 'prompt))

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
