;; -*- lexical-binding: t -*-
(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-popup-type 'side-window)
  )

(use-package general)

(general-create-definer global-definer
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
(general-create-definer global-leader
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ","
    "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

(with-eval-after-load 'evil
  (global-definer
    "f" '(:ignore t :which-key "file")
    "fF" 'find-file-other-window
    "fs" 'save-buffer
    "ff" 'find-file
    "s" '(:ignore t :which-key "search")
    "sp" 'consult-ripgrep
    "ss" 'consult-line
    "sj" 'evil-show-jumps
    "sm" 'evil-show-marks
    "sr" 'evil-show-registers
    "si" 'imenu
    "sf" 'consult-find
    "sd" 'consult-dir
    "g" '(:ignore t :which-key "magit")
    "gb" 'magit-blame
    "gc" 'magit-clone
    "gg" 'magit
    "b" '(:ignore t :which-key "buffer")
    "bb" 'consult-buffer
    "bB" 'consult-buffer-other-window
    "bz" 'bury-buffer
    "of" 'make-frame
    "oa" 'org-agenda
    "ot" 'treemacs
    "oo" 'olivetti-mode
    "p" '(projectile-command-map :which-key "projectile")
    "q" '(:ignore t :wk "quit")
    "qr" 'restart-emacs
    "qq" 'evil-quit-all))

  (with-eval-after-load 'org
    (global-leader 'org-mode
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
    (global-leader 'lsp-bridge-mode-map
      "a" 'lsp-bridge-code-action
      "d" 'lsp-bridge-find-define))



(provide 'init-keybindings)
