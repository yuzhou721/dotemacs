;; -*- lexical-binding: t; -*-
;;; Code:
(use-package org
  :pin melpa
  :ensure t
  :config
  (setq-default org-directory "~/org")
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
  :bind (:map org-mode-map
	      ("C-RET" . org-insert-subheading)
	      ("C-M-RET" . org-insert-todo-subheading)))

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
                        ("shell"  . sh)))
  (org-babel-load-languages '((C          . t)
                              (dot        . t)
                              (emacs-lisp . t)
                              (eshell     . t)
                              (python     . t)
                              (shell      . t))))

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
  :bind (("C-c n l" . org-roam-buffer-toggle)
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
  :ensure t)

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  )
(provide 'init-org)
;;; init-org.el ends here
