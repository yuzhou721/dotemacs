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

(defun org-make-logbooks-read-only ()
  "Read only logbooks."
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward
			"^ *:LOGBOOK:\n\\( *-.+\n\\)+ *:END:\n" nil t)
	  (add-text-properties (- (match-beginning 0) 1) (- (match-end 0) 1) '(read-only t)))))

(defun org-hide-properties ()
  "Hide all 'org-mode' headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward
			"^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
	  (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
		(overlay-put ov_this 'display "")
		(overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all 'org-mode' property drawers hidden by org-hide-properties."
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
	  (org-show-properties)
	(org-hide-properties)))

(use-package org
  :pin melpa
  :ensure t
  :hook
  (org-mode . (lambda () (org-make-logbooks-read-only)))
  (org-mode . (lambda () (org-hide-properties)))
  :custom
  (org-startup-indented t)
  (org-capture-last-stored nil)
  :config
  (setq org-directory "~/org")
  (setq-default org-agenda-files (list org-directory))
  (setq org-todo-keywords
		'((sequence
		   "TODO(t)"   ; A task that needs doing & is ready to do
		   "PROJ(p)"   ; A project, which usually contains other tasks
		   "LOOP(r)"   ; A recurring task
		   "STRT(s)"   ; A task that is in progress
		   "WAIT(w)"   ; Something external is holding up this task
		   "HOLD(h)"   ; This task is paused/on hold because of me
		   "IDEA(i)"   ; An unconfirmed and unapproved task or notion
		   "|"
		   "DONE(d)"					; Task successfully completed
		   "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
		  (sequence
		   "[ ](T)"					 ; A task that needs doing
		   "[-](S)"					 ; Task is in progress
		   "[?](W)"					 ; Task is being held up or paused
		   "|"
		   "[X](D)")					; Task was completed
		  ))
  (setq org-default-notes-file
		(expand-file-name +org-capture-notes-file org-directory)
		+org-capture-todo-file
		(expand-file-name +org-capture-todo-file org-directory)
		+org-capture-notes-file
		(expand-file-name +org-capture-notes-file org-directory)
		+org-capture-journal-file
		(expand-file-name +org-capture-journal-file org-directory)
		org-capture-templates
		'(("t" "Personal todo" entry
		   (file+headline +org-capture-todo-file "Inbox")
		   "* TODO %?\n%i\n%a" :prepend t)
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
  ;; 周一作为每周开始
  (setq org-agenda-start-on-weekday 1)
  (setq org-babel-load-languages '((C . t)
							  (dot . t)
							  (emacs-lisp . t)
							  (eshell . t)
							  (python . t)
							  (shell . t)
							  (java . t)
							  (clojure . t)))
  (setq org-babel-clojure-backend 'cider)
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
			  ("C-M-RET" . org-insert-todo-subheading)))

(defun org-apperance-evil-hack ()
  (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
  (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))

;; 显示自动隐藏的元素
(use-package org-appear
  :after org
  :hook
  (org-mode . org-appear-mode)
  (org-mode . org-apperance-evil-hack)
  :config
  (setq org-appear-trigger 'manual)
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t))

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
(use-package simple-httpd
  :ensure t
  :after org-roam)

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
  (org-src-lang-modes '(("C" . c)
						("C++" . c++)
						("bash" . sh)
						("cpp" . c++)
						("dot" . graphviz-dot) ;; was `fundamental-mode'
						("elisp" . emacs-lisp)
						("ocaml" . tuareg)
						("shell" . sh)
						("java" . java)))
  )

;;org-roam-ui
(use-package websocket
  :ensure t
  :after org-roam)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-dailies-directory "~/org/roam/daily/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n i" . org-roam-node-insert)
		 ("C-c n c" . org-roam-capture)
		 ;; Dailies
		 ("C-c n j" . org-roam-dailies-capture-today)
		 ;; Dailies
		 :map org-mode-map
		 ("C-M-i" . completion-at-point)
		 :map org-roam-dailies-map
		 ("Y" . org-roam-dailies-capture-yesterday)
		 ("T" . org-roam-dailies-capture-tomorrow))
  ;; :bind-keymap
  ;; ("C-c n d" . org-roam-dailies-map)
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
		   :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
		`(
		  ("d" "default" entry
		   "* %?"
		   :target (file+head "%<%Y-%m-%d>.org"
							  "#+title: %<%Y-%m-%d>\n#+filetags: :resource:\n")))))

(use-package org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :bind
  ("C-c n g" . org-roam-ui-open)
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
   ;; ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n s" . consult-org-roam-search))

(use-package olivetti
  :ensure t)

(use-package org-super-agenda
  :ensure t
  :config
  (setq org-super-agenda-groups
		'(;; Each group has an implicit boolean OR operator between its selectors.
		  (:name "Today"		  ; Optionally specify section name
				 :time-grid t	  ; Items that appear on the time grid
				 :anything
				 (:scheduled "today" :deadline "today")
				 )
		  (:name "Important"
				 ;; Single arguments given alone
				 ;; :tag "bills"
				 :priority "A")
		  ;; Set order of multiple groups at once
		  (:order-multi (2 (:name "Shopping in town"
								  ;; Boolean AND group matches items that match all subgroups
								  :and (:tag "shopping" :tag "@town"))
						   (:name "Food-related"
								  ;; Multiple args given in list with implicit OR
								  :tag ("food" "dinner"))
						   (:name "Personal"
								  :habit t
								  :tag "personal")
						   (:name "Space-related (non-moon-or-planet-related)"
								  ;; Regexps match case-insensitively on the entire entry
								  :and (:regexp ("space" "NASA")
												;; Boolean NOT also has implicit OR between selectors
												:not (:regexp "moon" :tag "planet")))))
		  (:name "Running" :todo ("STRT" "[-]"))
		  ;; Groups supply their own section names when none are given
		  (:name "等待" :todo ("WAITING" "HOLD") :order 8)	; Set order of this section
		  (:priority<= "B"
					   ;; Show this section after "Today" and "Important", because
					   ;; their order is unspecified, defaulting to 0. Sections
					   ;; are displayed lowest-number-first.
					   :order 1)
		  ;; After the last group, the agenda will display items that didn't
		  ;; match any of these groups, with the default order position of 99
		  ;; (org-agenda nil "a")
		  )
		)
  (org-super-agenda-mode)
  ;; Fix agenda header keymap error
  (setq org-super-agenda-header-map nil))

(use-package pangu-spacing
  :ensure t
  :hook (text-mode . pangu-spacing-mode)
  :config
  (add-hook 'org-mode-hook (lambda ()
							 (setq pangu-spacing-real-insert-separtor t))))

(provide 'init-org)
;;; init-org.el ends here
