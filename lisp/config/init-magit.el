(defvar extension-magit-dir (concat emacs-extensions-dir "/magit"))

(use-package compat
  :ensure t
  )

(use-package transient
  :ensure t)

(use-package dash
  :ensure t)

(use-package with-editor
  :ensure t)

(use-package magit
  :load-path extension-magit-dir
  )

(provide 'init-magit)
