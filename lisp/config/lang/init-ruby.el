;;; init-golang --- clojure -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; 使用rbenv
(use-package rbenv
  :ensure t
  :config
  (global-rbenv-mode)
  (rbenv-use "3.4.4"))

(use-package ruby-mode
  :mode "\\.\\(?:a?rb\\|aslsx\\)\\'"
  :mode "/\\(?:Brew\\|Fast\\)file\\'"
  :init
  (global-leader 'ruby-mode-map
    "[" 'ruby-toggle-block
    "{" 'ruby-toggle-block))

;; 添加html.erb支持
(with-eval-after-load 'web-mode
  (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . web-mode)))

(use-package enh-ruby-mode
  :ensure t
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :init
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'yard-mode)
  (setq enh-ruby-program (expand-file-name "~/.rbenv/shims/ruby")))

(use-package yard-mode
  :ensure t
  :hook ruby-mode)

(use-package rubocop
  :ensure t
  :hook (ruby-mode . rubocop-mode)
  :init
  (global-leader 'ruby-mode-map
    "f" 'rubocop-check-current-file
    "F" 'rubocop-autocorrect-current-file
    "p" 'rubocop-check-project
    "P" 'rubocop-autocorrect-project))

(use-package inf-ruby
  :ensure t
  :config
  (with-eval-after-load 'enh-ruby-mode
      (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode))
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter))

(use-package robe
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-ts-mode-hook 'robe-mode))

(use-package bundler
  :ensure t
  :init
  (global-leader 'ruby-mode-map
    "b" '(:ignore t :wk "bunder")
    "bc" 'bundle-check
    "bC" 'bundle-console
    "bi" 'bundle-install
    "bu" 'bundle-update
    "be" 'bundle-exec
    "bo" 'bundle-open))

(use-package rake
  :ensure t
  :init
  (global-leader 'ruby-mode-map
    "k" '(:ignore t :wk "rack")
    "kr" 'rake-rerun
    "kR" 'rake-regenerate-cache
    "kf" 'rake-find-task))

(use-package rails-routes
  :ensure t
  :general
  (:keymaps 'web-mode-map
            "C-c o" 'rails-routes-insert
            "C-c C-o" 'rails-routes-insert-no-cache
            "C-c ! o" 'rails-routes-jump)
  (:keymaps 'ruby-mode-map
            "C-c o" 'rails-routes-insert
            "C-c C-o" 'rails-routes-insert-no-cache
            "C-c ! o" 'rails-routes-jump))

(provide 'init-ruby)
;;; init-ruby.el ends here
