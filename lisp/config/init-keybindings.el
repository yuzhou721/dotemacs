;; -*- lexical-binding: t -*-
(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-popup-type 'side-window)
  )

(use-package evil
  :config
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'normal (kbd "<leader>m") :localleader)

  (evil-global-set-key 'normal (kbd "<leader>SPC") 'keyboard-escape-quit)

  ;;文件相關
  (which-key-add-key-based-replacements "<leader>f" "files")
  (evil-global-set-key 'normal (kbd "<leader>ff") 'find-file)
  (evil-global-set-key 'normal (kbd "<leader>fF") 'find-file-other-window)

  ;搜索
  (which-key-add-key-based-replacements "<leader>s" "search")
  (evil-global-set-key 'normal (kbd "<leader>sp") 'consult-ripgrep)
  (evil-global-set-key 'normal (kbd "<leader>ss") 'consult-line)

  (evil-global-set-key 'normal (kbd "<leader>p") 'projectile-command-map)

  ;magit
  (which-key-add-key-based-replacements "<leader>g" "search")
  )



(provide 'init-keybindings)
