;; -*- lexical-binding: t -*-
(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-popup-type 'side-window)
  )


   (defun define-leader-key (state map localleader &rest bindings)
      "Define leader key in MAP when STATE, a wrapper for
`evil-define-key*'. All BINDINGS are prefixed with \"<leader>\"
if LOCALLEADER is nil, otherwise \"<localleader>\"."
      (cl-assert (cl-evenp (length bindings)))
      (let ((prefix (if localleader "<localleader>" "<leader>"))
            wk-replacements)
        (while bindings
          (let ((key (pop bindings))
                (def (pop bindings)))
            (when (symbolp def)
              (evil-define-key* state map (kbd (concat prefix key)) def))
            ;; Save which-key (key . replacement).
            (pcase def
              (`(:wk ,replacement)
               (push (cons (concat prefix key) replacement) wk-replacements)))))
        ;; which-key integration.
        ;; XXX: replacement for localleader NOT supported.
        (with-eval-after-load 'which-key
          (cl-loop for (key . replacement) in wk-replacements
                   unless localleader
                   do (which-key-add-key-based-replacements key replacement)))))

(use-package evil
  :config

  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'normal (kbd "<leader>m") :localleader)

  (define-leader-key 'normal 'global nil
    "SPC" 'keyboard-escape-quit

    ;;文件相關
    "f" '(:wk "file")
    "ff" 'find-file
    "fF" 'find-file-other-window
    "fs" 'save-buffer

  ;搜索
    "s" '(:wk "search")
    "sp" 'consult-ripgrep
    "ss" 'consult-line
    "sj" 'evil-show-jumps
    "sm" 'evil-show-marks
    "sr" 'evil-show-registers
    "si" 'imenu
    "sf" 'consult-find
    "sd" 'consult-dir
;; tab
    "t" '(:wk "tab")
    "t9" 'tab-bar-switch-to-last-tab
    "tc" 'tab-bar-close-tab
    "tC" 'tab-bar-close-group-tabs
    "tg" 'tab-bar-change-tab-group
    "ti" 'tab-switcher
    "tn" 'tab-bar-new-tab
    "to" 'tab-bar-close-other-tabs
    "tt" 'tab-bar-switch-to-tab
    "tp" 'tab-bar-switch-to-recent-tab
    "tr" 'tab-bar-rename-tab

  ;magit
    "g" '(:wk "magit")
    "gb" 'magit-blame
    "gc" 'magit-clone
    "gg" 'magit

  ;;buffer
    "b" '(:wk "buffer")
    "bb" 'consult-buffer
    "bB" 'consult-buffer-other-window
    "bz" 'bury-buffer

    ;; projectile
    "p" 'projectile-command-map

    ;;window
    "w" 'evil-window-map
    "wx" 'kill-buffer-and-window
    "w/" 'split-window-horizontally
    "w-" 'split-window-vertically

    ;;open
    "o" '(:wk "open")
    "of" 'make-frame
    "oa" 'org-agenda
    "ot" 'treemacs
    "oo" 'olivetti-mode
    )
  (with-eval-after-load 'org
    (define-leader-key 'normal org-mode-map :localleader
        "." 'org-goto
        "a" 'org-archive-subtree
        "d" 'org-deadline
        "e" 'org-set-effort
        "f" 'org-footnote-action
        "l" 'org-lint
        "o" 'org-toggle-ordered-property
        "p" 'org-set-property
        "q" 'org-set-tags-command
        "r" 'org-refile
        "s" 'org-schedule
        "t" 'org-todo
        "T" 'org-todo-list
      ))
  (with-eval-after-load 'lsp-bridge
    (define-leader-key 'normal lsp-bridge-mode-map :localleader
      "a" 'lsp-bridge-code-action
      "d" 'lsp-bridge-find-define))

  )



(provide 'init-keybindings)
