;; -*- lexical-binding: t -*-
(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-popup-type 'side-window)
  )


   (defun define-leader-key (state map localleader &rest bindings)
      "Define leader key in MAP when STATE, a wrapper for
`evil-define-key*'. All BINDINGS are prefixed with \"<leader>\"
if LOCALLEADER is nil, otherwise \"<localleader>\"."
      (cl-assert (cl-evenp (length bindings)))
      (let ((prefix (if localleader "<localleader>" "<leader>"))
            wk-replacements)
        (while bindings
          (let ((key (pop bindings))
                (def (pop bindings)))
            (when (symbolp def)
              (evil-define-key* state map (kbd (concat prefix key)) def))
            ;; Save which-key (key . replacement).
            (pcase def
              (`(:wk ,replacement)
               (push (cons (concat prefix key) replacement) wk-replacements)))))
        ;; which-key integration.
        ;; XXX: replacement for localleader NOT supported.
        (with-eval-after-load 'which-key
          (cl-loop for (key . replacement) in wk-replacements
                   unless localleader
                   do (which-key-add-key-based-replacements key replacement)))))

(use-package evil
  :config

  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'normal (kbd "<leader>m") :localleader)

  
  (define-leader-key 'normal 'global nil
    "SPC" 'keyboard-escape-quit

    ;;文件相關
    "f" '(:wk "file")
    "ff" 'find-file
    "fF" 'find-file-other-window

  ;搜索
    "s" '(:wk "search")
    "sp" 'consult-ripgrep
    "ss" 'consult-line
    "sj" 'evil-show-jumps
    "sm" 'evil-show-marks
    "sr" 'evil-show-registers
    "si" 'imenu

  ;magit
    "g" '(:wk "magit")
    "gb" 'magit-blame
    "gc" 'magit-clone

    )

  )



(provide 'init-keybindings)
