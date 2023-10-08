;; -*- lexical-binding: t -*-
(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-popup-type 'side-window))

(use-package general
  :config
  (general-create-definer global-definer
    :prefix "SPC"
    :states 'normal
   	:keymaps 'override
    )

  (general-create-definer global-leader
    :prefix ","
    :states '(normal motion)
    "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
         :wrapping global-definer
         :prefix-map ',(intern (concat "+general-global-" name "-map"))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
        ,@body)))

  (global-definer
   "f" '(:ignore t :which-key "file")
   "fs" 'save-buffer
   "ff" 'find-file
   "fR" 'rename-visited-file
   "s" '(:ignore t :which-key "search")
   "sp" 'consult-ripgrep
   "ss" 'consult-line
   "sj" 'evil-show-jumps
   "sm" 'evil-show-marks
   "sr" 'evil-show-registers
   "si" 'imenu
   "sf" 'consult-find
   "sd" 'consult-dir
   "g" '(:ignore t :which-key "magit")
   "gb" 'magit-blame
   "gc" 'magit-clone
   "gg" 'magit
   "b" '(:ignore t :which-key "buffer")
   "bb" 'consult-buffer
   "bB" 'consult-buffer-other-window
   "bz" 'bury-buffer
   "o" '(:ignore t :which-key "open")
   "of" 'make-frame
   "oa" 'org-agenda
   "ot" 'treemacs
   "oo" 'olivetti-mode
   "os" 'scratch-buffer
   "q" '(:ignore t :wk "quit")
   "qr" 'restart-emacs
   "qq" 'evil-quit-all
   "h" '(:ignore t :wk "help")
   "hr" 'dd/reload-emacs-config
   "hp" 'desmond/open-config-dir
   "x" 'org-capture
   ";" 'dashboard-open)

  (general-define-key :states 'insert
	"C-;" 'rime-inline-ascii))


(defun desmond/open-config-dir ()
    "Open config dired")

(provide 'init-keybindings)
