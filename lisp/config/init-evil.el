;; -*- lexical-binding: t -*-
(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-keybinding nil)
  :hook (after-init . evil-mode)
  :config
  (evil-mode 1)
  (when (< emacs-major-version 28)
    (use-package undo-fu
      :ensure t))
  ;; Fix mouse-left click error
  (define-key evil-motion-state-map [down-mouse-1] nil)
  ;; Silence line out of range error.
  :custom
;; undo will never freeze my Emacs
  (evil-undo-system (if (>= emacs-major-version 28) 'undo-redo 'undo-fu))
  ;; Switch to the new window after splitting
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-interactive-search-highlight 'selected-window)
 ;; when `visual-line-mode' enabled, exchange j/k with gj/gk
  (evil-respect-visual-line-mode t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-fine-undo t)
  (evil-want-C-g-bindings t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-symbol-word-search t)
  (evil-want-C-u-scroll t)
  )

(use-package evil-surround
  :ensure t
  :after evil
  :hook (after-init . global-evil-surround-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-setup-debugger-keys nil)
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-unimpaired-want-repeat-mode-integration t)
  :config
  (evil-collection-init))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-nerd-commenter
  :config
  (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
  ;; (evilnc-default-hotkeys)
  )
;; 文件末尾
(provide 'init-evil)
;;; init-evil.el ends here
