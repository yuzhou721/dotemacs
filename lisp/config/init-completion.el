(use-package corfu
;; Optional customizations
  :init
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match t)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 80)
  (setq corfu-max-width 100)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 1)
  (setq corfu-on-exact-match nil)

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :config
  (global-corfu-mode)
  )

(use-package cape
  :init
    (setq cape-dabbrev-min-length 3)
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-tex)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
   ;; Bind dedicated completion commands
   :bind (("C-c p p" . completion-at-point) ;; capf
           ("C-c p t" . complete-tag)        ;; etags
           ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
           ("C-c p f" . cape-file)
           ("C-c p k" . cape-keyword)
           ("C-c p s" . cape-symbol)
           ("C-c p a" . cape-abbrev)
           ("C-c p i" . cape-ispell)
           ("C-c p l" . cape-line)
           ("C-c p w" . cape-dict)
           ("C-c p \\" . cape-tex)
           ("C-c p _" . cape-tex)
           ("C-c p ^" . cape-tex)
           ("C-c p &" . cape-sgml)
           ("C-c p r" . cape-rfc1345))
  )

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
  ;; make c-j/c-k work in vertico selection
  :bind
  (:map vertico-map
   ("C-j" . 'vertico-next)
   ("C-k" . 'vertico-previous))
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
  (:map minibuffer-local-map
   ("M-o" . 'embark-act)         ;; pick some comfortable binding
   ("C-c C-c" . 'embark-export)        ;; good alternative: M-.
   ("C-c C-o" . 'embark-collect)) ;; alternative for `describe-bindings'
  ;;:init
  ;; Optionally replace the key help with a completing-read interface
  ;;(setq #'embark-prefix-help-command)
  )

(use-package consult
  :ensure t
  :bind (([remap imenu]                  . consult-imenu)
         ([remap goto-line]              . consult-goto-line)
         ([remap bookmark-jump]          . consult-bookmark)
         ([remap evil-show-marks]        . consult-mark)
         ([remap recentf-open-files]     . consult-recent-file)
         ([remap repeat-complex-command] . consult-complex-command)))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :after embark consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package pyim
  :after orderless
  :config
  (require 'pyim-cregexp-utils)
  (defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result)))

  (defun toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
	(advice-add 'orderless-regexp :around #'eh-orderless-regexp)
      (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  (defun disable-py-search (&optional args)
    (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
	(advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  ;; (advice-add 'exit-minibuffer :after #'disable-py-search)
  :hook
  ('minibuffer-exit-hook 'disable-py-search)
  )

(provide 'init-completion)
