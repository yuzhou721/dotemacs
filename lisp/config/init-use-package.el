
;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(require 'use-package)
(require 'use-package-ensure)
(provide 'init-use-package)
