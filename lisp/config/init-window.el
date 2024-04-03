(use-package winner
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
				      "*Compile-Log*"
				      "*inferior-lisp*"
				      "*Fuzzy Completions*"
				      "*Apropos*"
				      "*Help*"
				      "*cvs*"
				      "*Buffer List*"
				      "*Ibuffer*"
				      "*esh command on file*")))

(use-package resize-window
  :ensure t
  :init
  (defvar resize-window-dispatch-alist
    '((?n resize-window--enlarge-down " Resize - Expand down" t)
      (?p resize-window--enlarge-up " Resize - Expand up" t)
      (?f resize-window--enlarge-horizontally " Resize - horizontally" t)
      (?b resize-window--shrink-horizontally " Resize - shrink horizontally" t)
      (?r resize-window--reset-windows " Resize - reset window layout" nil)
      (?w resize-window--cycle-window-positive " Resize - cycle window" nil)
      (?W resize-window--cycle-window-negative " Resize - cycle window" nil)
      (?2 split-window-below " Split window horizontally" nil)
      (?3 split-window-right " Slit window vertically" nil)
      (?0 resize-window--delete-window " Delete window" nil)
      (?K resize-window--kill-other-windows " Kill other windows (save state)" nil)
      (?y resize-window--restore-windows " (when state) Restore window configuration" nil)
      (?? resize-window--display-menu " Resize - display menu" nil))
    "List of actions for `resize-window-dispatch-default.
Main data structure of the dispatcher with the form:
\(char function documentation match-capitals\)"))

(use-package popper
  :defines opper-echo-dispatch-actions
  :commands popper-group-by-directory
  :bind (:map popper-mode-map
			  ("M-=" . popper-toggle-latest)
			  ("C-`" . popper-toggle-latest)
			  ;; ("s-o" . popper-cycle)
			  ;; ("M-`" . popper-toggle-type)
			  )
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
		'("\\*Messages\\*"
		  "Output\\*$" "\\*Pp Eval Output\\*$"
		  "\\*Compile-Log\\*"
		  "\\*Completions\\*"
		  "\\*Warnings\\*"
		  "\\*Flymake diagnostics.*\\*"
		  "\\*Async Shell Command\\*"
		  "\\*Apropos\\*"
		  "\\*Backtrace\\*"
		  "\\*prodigy\\*"
		  "\\*Calendar\\*"
		  "\\*Embark Actions\\*"
		  "\\*Finder\\*"
		  "\\*Kill Ring\\*"
		  "\\*Embark Export:.*\\*"
		  "\\*Edit Annotation.*\\*"
		  "\\*Flutter\\*"
		  "\\*lsp-bridge\\*"
		  bookmark-bmenu-mode
		  lsp-bridge-ref-mode
		  comint-mode
		  compilation-mode
		  help-mode helpful-mode
		  tabulated-list-mode
		  Buffer-menu-mode
		  occur-mode
		  gnus-article-mode devdocs-mode
		  grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
		  ivy-occur-mode ivy-occur-grep-mode
		  process-menu-mode list-environment-mode cargo-process-mode
		  youdao-dictionary-mode osx-dictionary-mode fanyi-mode

		  "^\\*eshell.*\\*.*$" eshell-mode
		  "^\\*shell.*\\*.*$" shell-mode
		  "^\\*terminal.*\\*.*$" term-mode
		  "^\\*vterm.*\\*.*$" vterm-mode

		  "\\*DAP Templates\\*$" dap-server-log-mode
		  "\\*ELP Profiling Restuls\\*" profiler-report-mode
		  "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
		  "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
		  "\\*[Wo]*Man.*\\*$"
		  "\\*ert\\*$" overseer-buffer-mode
		  "\\*gud-debug\\*$"
		  "\\*lsp-help\\*$" "\\*lsp session\\*$"
		  "\\*quickrun\\*$"
		  "\\*tldr\\*$"
		  "\\*vc-.*\\*$"
		  "\\*eldoc\\*"
		  "\\*General Keybindings\\*"
		  "^\\*elfeed-entry\\*$"
		  "^\\*macro expansion\\**"
          "\\*sly-mrepl*"

		  "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
		  "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
		  "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
		  "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
		  "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
		  rustic-cargo-outdated-mode rustic-cargo-test-moed
		  cider-repl-mode
          "\\*Moonshot\\*"))

  (when (display-grayscale-p)
	(setq popper-mode-line
		  '(:eval
			(concat
			 (propertize " " 'face 'mode-line-emphasis)
			 (all-the-icons-octicon "pin" :height 0.9 :v-adjust 0.0 :face 'mode-line-emphasis)
			 (propertize " " 'face 'mode-line-emphasis)))))

  (setq popper-echo-dispatch-actions t)
  (setq popper-group-function nil)
  (setq popper-group-function #'popper-group-by-project) ; project.el projects
  :config
  (require 'popper-echo)
  (popper-echo-mode 1)

  (with-no-warnings
	(defun my-popper-fit-window-height (win)
	  "Determine the height of popup window WIN by fitting it to the buffer's content."
	  (fit-window-to-buffer
	   win
	   (floor (frame-height) 3)
	   (floor (frame-height) 3)))
	(setq popper-window-height #'my-popper-fit-window-height)

	(defun popper-close-window-hack (&rest _)
	  "Close popper window via `C-g'."
	  ;; `C-g' can deactivate region
	  (when (and (called-interactively-p 'interactive)
				 (not (region-active-p))
				 popper-open-popup-alist)
		(let ((window (caar popper-open-popup-alist)))
		  (when (window-live-p window)
			(delete-window window)))))
	(advice-add #'keyboard-quit :before #'popper-close-window-hack)))

(use-package buffer-move
  :ensure t)

(use-package es-windows
  :ensure t)

(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :init
  (setq shackle-lighter "")
  (setq shackle-select-reused-windows nil) ; default nil
  (setq shackle-default-alignment 'below)  ; default below
  (setq shackle-rules
        ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
        '((compilation-mode              :ignore t)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          ("\\*corfu.*\\*"       :regexp t :ignore t)
          ("*eshell*"                    :select t                          :size 0.4  :align t     :popup t)
          (eshell-mode                    :select t                          :size 0.4  :align t     :popup t)
          ("\\*shell*"                    :select t                          :size 0.4  :align t     :popup t)
          (shell-mode                    :select t                          :size 0.4  :align t     :popup t)
          ("*Moonshot*"                  :select t                          :size 0.4  :align t     :popup t)
          (helpful-mode                  :select t                          :size 0.6  :align right :popup t)
          ("*Messages*"                  :select t                          :size 0.4  :align t     :popup t)
          ("*Calendar*"                  :select t                          :size 0.3  :align t     :popup t)
          ("*info*"                      :select t                                                  :same t)
          (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
          (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
          (cider-repl-mode               :select nil                          :size 0.4  :align t     :popup t)
          ("\\*sly-macroexpansion\\*" :select nil :inhibit-window-quit t :size 0.4 :align t :same t)
          ))
  )

;; key binding
(+general-global-menu! "window" "w"
  "/" 'split-window-right
  "-" 'split-window-below
  "m" 'delete-other-windows
  "u" 'winner-undo
  "z" 'winner-redo
  "w" 'esw/select-window
  "s" 'esw/swap-two-windows
  "d" 'esw/delete-window
  "=" 'balance-windows-area
  "r" 'esw/move-window
  "x" 'resize-window
  "H" 'buf-move-left
  "L" 'buf-move-right
  "J" 'buf-move-down
  "K" 'buf-move-up)


(provide 'init-window)
