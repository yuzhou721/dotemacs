;;; init-clojure --- clojure -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package clojure-mode
  :ensure t
  :pin melpa
  :hook
  ('clojure-mode . 'lispy-mode))

(defun cider-debug-evil-hack ()
  "处理 debug 时候 evil 按键冲突问题"
  (with-eval-after-load 'evil
	(add-hook 'cider--debug-mode (lambda ()
								   (evil-make-overriding-map cider--debug-mode-map 'normal)
								   (evil-normalize-keymaps)))))

(use-package cider
  :ensure t
  :pin melpa
  :config
  (cider-debug-evil-hack)
  ;; Repl-mode evil initial state
  ;; (evil-set-initial-state 'cider-repl-mode 'emacs)
  (setq cider-repl-history-file (expand-file-name "cider-repl-history" emacs-extension-cache-dir))
  :general
  (global-leader 'clojure-mode-map
	"'" 'cider-jack-in-clj
	"\"" 'cider-jack-in-cljs
	"c" 'cider-connect-clj
	"C" 'cider-connect-cljs
	"m" 'cider-macroexpand-1
	"M" 'cider-macroexpand-all
	"d" '(:ignore t :wk "debug")
	"dd" 'cider-debug-defun-at-point
	"e" '(:ignore t :wk "eval")
	"eb" 'cider-eval-buffer
	"ed" 'cider-eval-defun-at-point
	"eD" 'cider-insert-defun-in-repl
	"ee" 'cider-eval-last-sexp
	"eE" 'cider-insert-last-sexp-in-repl
	"er" 'cider-eval-region
	"eR" 'cider-insert-region-in-repl
	"eu" 'cider-undef
	"g" '(:ignore t :wk "goto")
	"gb" 'cider-pop-back
	"gg" 'cider-find-var
	"gn" 'cider-find-ns
	"h" '(:ignore t :wk "help")
	"hn" 'cider-find-ns
	"ha" 'cider-apropos
	"hc" 'cider-clojuredocs
	"hd" 'cider-doc
	"hj" 'cider-javadoc
	"hw" 'cider-clojuredocs-web
	"i" '(:ignore t :wk "inspect")
	"ie" 'cider-enlighten-mode
	"ii" 'cider-inspect
	"ir" 'cider-inspect-last-result
	"n" '(:ignore t :wk "namespace")
	"nn" 'cider-browse-ns
	"nN" 'cider-browse-ns-all
	"nr" 'cider-ns-refresh
	"p" '(:ignore t :wk "print")
	"pp" 'cider-pprint-eval-last-sexp
	"pP" 'cider-pprint-eval-last-sexp-to-comment
	"pd" 'cider-pprint-eval-defun-at-point
	"pD" 'cider-pprint-eval-defun-to-comment
	"pr" 'cider-pprint-eval-last-sexp-to-repl
	"r" '(:ignore t :wk "repl")
	"rn" 'cider-repl-set-ns
	"rq" 'cider-quit
	"rr" 'cider-ns-refresh
	"rR" 'cider-restart
	"rb" 'cider-switch-to-repl-buffer
	;; "rB" '+clojure/cider-switch-to-repl-buffer-and-switch-ns
	"rc" 'cider-find-and-clear-repl-output
	"rl" 'cider-load-buffer
	"rL" 'cider-load-buffer-and-switch-to-repl-buffer
	"t" '(:ignore t :wk "test")
	"ta" 'cider-test-rerun-test
	"tl" 'cider-test-run-loaded-tests
	"tn" 'cider-test-run-ns-tests
	"tp" 'cider-test-run-project-tests
	"tr" 'cider-test-rerun-failed-tests
	"ts" 'cider-test-run-ns-tests-with-filters
	"tt" 'cider-test-run-test)
  (global-leader 'cider-repl-mode-map
	"n" 'cider-repl-set-ns
	"q" 'cider-quit
	"r" 'cider-ns-refresh
	"R" 'cider-restart
	"c" 'cider-repl-clear-buffer)
  (:keymaps 'cider-repl-mode-map :states 'insert
			[S-return] 'cider-repl-newline-and-indent
			[M-return] 'cider-repl-return)
  (:keymaps 'cider-repl-history-mode-map :states 'insert
			[return] 'cider-repl-history-insert-and-quit
			"q" 'cider-repl-history-quit
			"l" 'cider-repl-history-occur
			"s" 'cider-repl-history-search-forward
			"r" 'cider-repl-history-search-backward
			"U" 'cider-repl-history-undo-other-window))

(provide 'init-clojure)
