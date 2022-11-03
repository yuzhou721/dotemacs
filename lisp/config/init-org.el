-*- lexical-binding: t; -*-

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
           "NO(n)"))))

(use-package org-attach
  :ensure t
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
      (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir))

;; TODO Move to +encrypt flag
(use-package org-crypt ; built-in
  :ensure t
  :commands org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry
  :hook (org-reveal-start . org-decrypt-entry)
  :preface
  ;; org-crypt falls back to CRYPTKEY property then `epa-file-encrypt-to', which
  ;; is a better default than the empty string `org-crypt-key' defaults to.
  (defvar org-crypt-key nil)
  (after! org
    (add-to-list 'org-tags-exclude-from-inheritance "crypt")
    (add-hook 'org-mode-hook
      (add-hook 'before-save-hook 'org-encrypt-entries nil t))))
