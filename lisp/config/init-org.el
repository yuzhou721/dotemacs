;; -*- lexical-binding: t; -*-
;;; Code:
(defvar +org-capture-todo-file "todo.org"
  "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-changelog-file "changelog.org"
  "Default target for changelog entries.

Is relative to `org-directory' unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-notes-file "notes.org"
  "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that do not specify a
target file.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-journal-file "journal.org"
  "Default target for storing timestamped journal entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-projects-file "projects.org"
  "Default, centralized target for org-capture templates.")


(use-package org
  :pin melpa
  :ensure t
  :custom
  (org-startup-indented t)
  (org-capture-last-stored nil)
  :config
  ; todo
  (setq org-directory "~/org")
  (setq-default org-agenda-files (list org-directory))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)")))
  (setq org-default-notes-file
	(expand-file-name +org-capture-notes-file org-directory)
	+org-capture-journal-file
	(expand-file-name +org-capture-journal-file org-directory)
	org-capture-templates
	'(("t" "Personal todo" entry
	   (file+headline org-default-notes-file "Inbox")
	   "* [ ] %?\n%i\n%a" :prepend t)
	  ("n" "Personal notes" entry
	   (file+headline +org-capture-notes-file "Inbox")
	   "* %u %?\n%i\n%a" :prepend t)
	  ("j" "Journal" entry
	   (file+olp+datetree +org-capture-journal-file)
	   "* %U %?\n%i\n%a" :prepend t)))
  (setq org-refile-targets
	'((nil :maxlevel . 3)
	  (org-agenda-files :maxlevel . 3))
	;; Without this, completers like ivy/helm are only given the first level of
	;; each outline candidates. i.e. all the candidates under the "Tasks" heading
	;; are just "Tasks/". This is unhelpful. We want the full path to each refile
	;; target! e.g. FILE/Tasks/heading/subheading
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil)
  :general
  (global-leader 'org-mode-map
	"#" 'org-update-statistics-cookies
        "'" 'org-edit-special
        "*" 'org-ctrl-c-star
        "+" 'org-ctrl-c-minus
        "," 'org-switchb
        "." 'org-goto
        "@" 'org-cite-insert
	"." 'consult-org-heading
        "/" 'consult-org-agenda
        "A" 'org-archive-subtree
        "e" 'org-export-dispatch
        "f" 'org-footnote-action
        "h" 'org-toggle-heading
        "i" 'org-toggle-item
        "I" 'org-id-get-create
        "k" 'org-babel-remove-result
        "n" 'org-store-link
        "o" 'org-set-property
        "q" 'org-set-tags-command
        "t" 'org-todo
        "T" 'org-todo-list
        "x" 'org-toggle-checkbox
	"a" '(:ignore t :wk "attachments")
        "aa" 'org-attach
        "ad" 'org-attach-delete-one
        "aD" 'org-attach-delete-all
        "an" 'org-attach-new
        "ao" 'org-attach-open
        "aO" 'org-attach-open-in-emacs
        "ar" 'org-attach-reveal
        "aR" 'org-attach-reveal-in-emacs
        "au" 'org-attach-url
        "as" 'org-attach-set-directory
        "aS" 'org-attach-sync)
  :bind (:map org-mode-map
	      ("C-RET" . org-insert-subheading)
	      ("C-M-RET" . org-insert-todo-subheading)
	      ))

(defun org-apperance-evil-hack ()
  (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
  (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))

;; 显示自动隐藏的元素
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook 'org-apperance-evil-hack))

;; 中国日历
(use-package cal-china-x
  :demand t
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
  (setq calendar-holidays
	(append cal-china-x-important-holidays
		cal-china-x-general-holidays)))

(use-package org-attach
  :ensure nil
  :after org
    :commands (org-attach-new
               org-attach-open
               org-attach-open-in-emacs
               org-attach-reveal-in-emacs
               org-attach-url
               org-attach-set-directory
               org-attach-sync)
    :config
    (unless org-attach-id-dir
      ;; Centralized attachments directory by default
      (setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory)))
    ;; (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir)
    )

(use-package org-crypt ; built-in
  :ensure nil
  :commands (org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry)
  :hook (org-reveal-start . org-decrypt-entry)
  :config
  ;; org-crypt falls back to CRYPTKEY property then `epa-file-encrypt-to', which
  ;; is a better default than the empty string `org-crypt-key' defaults to.
  (setq org-crypt-key "shoper2@163.com")
  (add-to-list 'org-tags-exclude-from-inheritance "crypt" t)
  )

;; Write codes in org-mode
(use-package org-src
  :ensure nil
  :after org
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (:map org-src-mode-map
         ;; consistent with separedit/magit
         ("C-c C-c" . org-edit-src-exit))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'other-window)
  (org-src-lang-modes '(("C"      . c)
                        ("C++"    . c++)
                        ("bash"   . sh)
                        ("cpp"    . c++)
                        ("dot"    . graphviz-dot) ;; was `fundamental-mode'
                        ("elisp"  . emacs-lisp)
                        ("ocaml"  . tuareg)
                        ("shell"  . sh)
			("java" . java)))
  (org-babel-load-languages '((C          . t)
                              (dot        . t)
                              (emacs-lisp . t)
                              (eshell     . t)
                              (python     . t)
                              (shell      . t)
			      (java . t)
			      (clojure . t)
			      )))



(use-package simple-httpd
  :ensure t
  :after org-roam)

;;org-roam-ui
(use-package websocket
  :ensure t
  :after org-roam)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-dailies-directory "~/org/roam/daily/")
  :bind (("C-c n t" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n g" . org-roam-ui-open)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  ;; crypt encode
  (org-crypt-use-before-save-magic)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-dailies)
  (setq org-roam-capture-ref-templates
        '(
          ("r" "ref" plain
           "* %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :web:resource:\n Original Reference: [[${ref}][${title}]]\n -----\n")
           :unnarrowed t)
          ("a" "Annotation" plain
           "#+begin_quote \n ${body}\n #+end_quote \n %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :web:resource:\n Original Reference: [[${ref}]][${title}]\n -----\n")
           :immediate-finish t
           :empty-lines 1
           :unnarrowed t)
          ("o" "Copy from clipboard" plain
           "%x"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :web:resource:\n Original Reference: [[${ref}]][${title}]\n -----\n")
           :immediate-finish t
           :empty-lines 1
           :unnarrowed t)
          )
        )
  (setq org-roam-dailies-capture-templates
        `(
          ("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :resource:\n"
                              ))))
  )

(use-package org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package org-protocol
  :ensure nil)

(use-package org-roam-protocol
  :after org-protocol)

(use-package evil-org
  :ensure t
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :after org
  )

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  )

(use-package consult-org-roam
   :ensure nil
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n F" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n s" . consult-org-roam-search))

(use-package olivetti
  :ensure t)

(provide 'init-org)
;;; init-org.el ends here
