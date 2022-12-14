;; -*- lexical-binding: t -*-
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

;; (use-package pyim
;;   :ensure t
;;   :pin gnu
;;   :config
;;   (require 'pyim-cregexp-utils)
;;   (defun eh-orderless-regexp (orig_func component)
;;     (let ((result (funcall orig_func component)))
;;       (pyim-cregexp-build result)))

;;   (defun toggle-chinese-search ()
;;     (interactive)
;;     (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
;; 	(advice-add 'orderless-regexp :around #'eh-orderless-regexp)
;;       (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

;;   (defun disable-py-search (&optional args)
;;     (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
;; 	(advice-remove 'orderless-regexp #'eh-orderless-regexp)))

;;   ;; (advice-add 'exit-minibuffer :after #'disable-py-search)
;;   :hook
;;   ('minibuffer-exit-hook 'disable-py-search)
;;   )

;;???lsp-bridge??????
;; (use-package corfu
;; ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   :hook ((prog-mode . corfu-mode)
;;          (shell-mode . corfu-mode)
;;          (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-excluded-modes'.
;;   :config
;;   (global-corfu-mode)
;;   )
(provide 'init-minibuffer)
