;; -*- lexical-binding: t -*-
;;modeline上显示我的所有的按键和执行的命令
(use-package keycast
  :ensure t
  :config
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  :init
  (keycast-mode t)
  )

;; 这里的执行顺序非常重要，doom-modeline-mode 的激活时机一定要在设置global-mode-string 之后‘
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode t))

(use-package all-the-icons
  :ensure t
  :when (display-graphic-p)
  :demand t)

(use-package doom-themes
  :ensure t
  :when (display-graphic-p)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package emacs
  :ensure nil
  :unless (display-graphic-p)
  :config
  (load-theme 'leuven t))

(use-package dashboard
  :ensure t
  :init
  ;;Format: "(icon title help action face prefix suffix)"
  ;; (setq dashboard-navigator-buttons `(((,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "mark-github"      :height 1.0 :v-adjust  0.0) "★")
  ;;                                       "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
  ;;                                      (,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "heart"            :height 1.1 :v-adjust  0.0) "♥")
  ;;                                       "Stars" "Show stars" (lambda (&rest _) (browse-url stars-url)))
  ;;                                      (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "report_problem" :height 1.1 :v-adjust -0.2) "⚑")
  ;;                                       "Issue" "Report issue" (lambda (&rest _) (browse-url issue-url)) warning)
  ;;                                      (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "update"         :height 1.1 :v-adjust -0.2) "♺")
  ;;                                       "Update" "Update packages synchronously" (lambda (&rest _) (package-update-all nil)) success))))
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :hook ((after-init . dashboard-setup-startup-hook)
         (dashboard-mode . (lambda ()
                             (setq-local global-hl-line-mode nil))))
  ;; :config
  ;; (defconst homepage-url "https://github.com/condy0919/.emacs.d")
  ;; (defconst stars-url (concat homepage-url "/stargazers"))
  ;; (defconst issue-url (concat homepage-url "/issues/new"))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents   . 10)
                     (bookmarks . 5))))


;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)
  ;; Different scroll margin
  ;; (vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-resize t)
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless)))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode t)
  )

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  )

(use-package consult
  :ensure t
  :bind
  ("C-s" . consult-line)
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-ui)