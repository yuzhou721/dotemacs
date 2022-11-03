;; -*- lexical-binding: t -*-
;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; 显示行号
(global-linum-mode 1)

;; 更改光标的样式（不能生效，解决方案见第二集）
;;(setq cursor-type 'bar)

;;(icomplete-mode 1)
;; 禁止生成～备份文件
(setq make-backup-files nil
      auto-save-default nil)

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Always load the newest file
(setq load-prefer-newer t)

;; Cutting and pasting use primary/clipboard
(setq select-enable-primary t
      select-enable-clipboard t)

;; Highlight parenthesises
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package simple
  :ensure nil
  :custom
  ;; show line/column/filesize in modeline
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  ;; No visual feedback on copy/delete.
  (copy-region-blink-delay 0)
  (delete-pair-blink-delay 0)
  ;; confusing if no fringes (GUI only).
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  ;; don't save current clipboard text before replacing it
  (save-interprogram-paste-before-kill nil)
  ;; eliminate duplicates
  (kill-do-not-save-duplicates t)
  ;; include '\n' when point starts at the beginning-of-line
  (kill-whole-line t)
  ;; show cwd when `shell-command' and `async-shell-command'
  (shell-command-prompt-show-cwd t)
  ;; show the name of character in `what-cursor-position'
  (what-cursor-show-names t))

;; Type text
(use-package text-mode
  :ensure nil
  :custom
  ;; better word wrapping for CJK characters
  (word-wrap-by-category t)
  ;; paragraphs
  (sentence-end-double-space nil))

;; Back to the previous position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Highlight current line in GUI
(use-package hl-line
  :ensure nil
  :when (display-graphic-p)
  :hook (after-init . global-hl-line-mode))

;; Server mode.
;; Use emacsclient to connect
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; Build regexp with visual feedback
(use-package re-builder
  :ensure nil
  :commands re-builder
  :bind (:map reb-mode-map
         ("C-c C-k" . reb-quit)
         ("C-c C-p" . reb-prev-match)
         ("C-c C-n" . reb-next-match))
  :custom
  (reb-re-syntax 'string))

;; window layout manager
;;
;; gt next-tab
;; gT prev-tab
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-all)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

(use-package newcomment
  :ensure nil
  :bind ([remap comment-dwim] . comment-or-uncomment)
  :config
  (defun comment-or-uncomment ()
    "Comment or uncomment the current line or region.
If the region is active and `transient-mark-mode' is on, call
`comment-or-uncomment-region'.
Else, if the current line is empty, insert a comment and indent
it.
Else, call `comment-or-uncomment-region' on the current line."
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (comment-dwim nil)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  ;; `auto-fill' inside comments.
  ;;
  ;; The quoted text in `message-mode' are identified as comments, so only
  ;; quoted text can be `auto-fill'ed.
  (comment-auto-fill-only-comments t))

(use-package recentf
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-menu-item 10)
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  )

;; Workaround with minified source files
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; transparent remote access
(use-package tramp
  :ensure nil
  :defer t
  :custom
  ;; Always use file cache when using tramp
  (remote-file-name-inhibit-cache nil)
  (tramp-default-method "ssh"))

;; Buffer index
(use-package imenu
  :hook (imenu-after-jump . recenter))

;; Needed by `webpaste'
(use-package browse-url
  :ensure nil
  :custom
  (browse-url-generic-program (or (executable-find "firefox")
                                  (executable-find "chromium")
                                  (executable-find "google-chrome-stable")
                                  (executable-find "google-chrome")
                                  (when (eq system-type 'darwin) "open")
                                  (when (eq system-type 'gnu/linux) "xdg-open")))
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

;; Buffer manager
;;
;; `sR': switch to saved filter groups
(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind ([remap list-buffers] . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-movement-cycle nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("Default"
      ("Emacs" (or (name . "\\*scratch\\*")
                   (name . "\\*dashboard\\*")
                   (name . "\\*compilation\\*")
                   (name . "\\*Backtrace\\*")
                   (name . "\\*Packages\\*")
                   (name . "\\*Messages\\*")
                   (name . "\\*Customize\\*")))
      ("Browser" (or (mode . eww-mode)
                     (mode . xwidget-webkit-mode)))
      ("Help" (or (name . "\\*Help\\*")
                  (name . "\\*Apropos\\*")
                  (name . "\\*info\\*")
                  (mode . Man-mode)
                  (mode . woman-mode)))
      ("Repl" (or (mode . gnuplot-comint-mode)
                  (mode . inferior-emacs-lisp-mode)
                  (mode . inferior-python-mode)))
      ("Term" (or (mode . term-mode)
                  (mode . shell-mode)
                  (mode . eshell-mode)))
      ("Mail" (or (mode . mail-mode)
                  (mode . message-mode)
                  (derived-mode . gnus-mode)))
      ("Conf" (or (mode . yaml-mode)
                  (mode . conf-mode)))
      ("Dict" (or (mode . fanyi-mode)
                  (mode . dictionary-mode)))
      ("Text" (and (derived-mode . text-mode)
                   (not (starred-name))))
      ("Magit" (or (mode . magit-repolist-mode)
                   (mode . magit-submodule-list-mode)
                   (mode . git-rebase-mode)
                   (derived-mode . magit-section-mode)
                   (mode . vc-annotate-mode)))
      ("VC" (or (mode . diff-mode)
                (derived-mode . log-view-mode)))
      ("Prog" (and (derived-mode . prog-mode)
                   (not (starred-name))))
      ("Dired" (mode . dired-mode))
      ("IRC" (or (mode . rcirc-mode)
                 (mode . erc-mode)))))))

;; Notifications
;;
;; Actually, `notify-send' is not defined in notifications package, but the
;; autoload cookie will make Emacs load `notifications' first, then our
;; `defalias' will be evaluated.
(pcase system-type
  ('gnu/linux
   (use-package notifications
     :ensure nil
     :commands notify-send
     :config
     (defalias 'notify-send 'notifications-notify)))
  ('darwin
   (defun notify-send (&rest params)
     "Send notifications via `terminal-notifier'."
     (let ((title (plist-get params :title))
           (body (plist-get params :body)))
       (start-process "terminal-notifier"
                      nil
                      "terminal-notifier"
                      "-group" "Emacs"
                      "-title" title
                      "-message" body
                      "-activate" "org.gnu.Emacs"))))
  (_
   (defalias 'notify-send 'ignore)))


;; Try out emacs package without installing
(use-package try
  :ensure t
  :commands try try-and-refresh)


(provide 'init-better-defaults)
