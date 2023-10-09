
(use-package corfu
;; Optional customizations
  :init
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match t)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 80)
  (setq corfu-max-width 130)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 1)
  (setq corfu-on-exact-match nil)

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :config
  (global-corfu-mode)
  )

(use-package nerd-icons-corfu
  :ensure t
  :after nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(progn
  (defvar use-package--warning224
	(function
	 (lambda
	   (keyword err)
	   (let
		   ((msg
			 (format "%s/%s: %s" 'cape keyword
					 (error-message-string err))))
		 (display-warning 'use-package msg :error)))))
  (condition-case-unless-debug err
	  (progn
		(condition-case-unless-debug err
			(progn
			  (setq cape-dabbrev-min-length 3)
			  (add-to-list 'completion-at-point-functions
						   (function cape-dabbrev))
			  (add-to-list 'completion-at-point-functions
						   (function cape-file))
			  (add-to-list 'completion-at-point-functions
						   (function cape-dict)))
		  (error
		   (funcall use-package--warning224 :init err)))
		(if
			(not
			 (require 'cape nil t))
			(display-warning 'use-package
							 (format "Cannot load %s" 'cape)
							 :error)))
	(error
	 (funcall use-package--warning224 :catch err))))

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
  :config
  (setq completion-styles '(orderless basic)))

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

(use-package yasnippet
  :ensure nil
  :config
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(provide 'init-completion)
