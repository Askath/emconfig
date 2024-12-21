(setq which-key-sort-order 'which-key-key-order-alpha)
(setq which-key-max-display-columns 2)
(setq which-key-popup-type 'side-window)
(setq which-key-side-window-location 'right)
(setq which-key-side-window-max-height 0.25)
(setq which-key-side-window-max-width 0.33)

(which-key-add-key-based-replacements "C-x 3" "Open Split")
(which-key-add-key-based-replacements "C-x 1" "Close all but this")
(which-key-add-key-based-replacements "C-x 1" "Close this")

(which-key-add-key-based-replacements "C-x C-f" "[F]ile")
(define-prefix-command 'file-prefix-map)
(global-set-key (kbd "C-x C-f") 'file-prefix-map)
(define-key file-prefix-map (kbd "f") 'find-file)           ; Open file
(define-key file-prefix-map (kbd "r") 'recentf)            ; Open recent files

(global-set-key (kbd "C-=") (cons "Expand Selection" 'er/expand-region))
(global-set-key (kbd "s-v") 'cua-paste)
(global-set-key (kbd "C-v") 'scroll-up-command)
(global-set-key (kbd "M-s g") 'grep-find)
(global-set-key (kbd "C-x ,") 'ibuffer)
(global-set-key (kbd "C-c a") 'embark-act)
(global-set-key (kbd "M-i") 'imenu)

  (which-key-add-key-based-replacements "C-c m" "[M]ajor mode")
  (which-key-add-key-based-replacements "C-c o" "[O]pen")
  (which-key-add-key-based-replacements "C-c c" "[C]ode")
  (which-key-add-key-based-replacements "C-c j" "[J]ump to")
  (which-key-add-key-based-replacements "C-c l" "[L]edger")
  (which-key-add-key-based-replacements "C-c n" "[N]otes")
(which-key-add-key-based-replacements "C-c x" "[X] Capture")

  (define-prefix-command 'gptel-prefix-map)
  (global-set-key (kbd "C-c A") 'gptel-prefix-map)
  ;; (which-key-add-key-based-replacements "C-c A" "[A]I Chat")
  (define-key gptel-prefix-map (kbd "m") (cons "Open AI [M]enu" 'gptel-menu))
  (define-key gptel-prefix-map (kbd "s") (cons "[S]end to AI" 'gptel-send))
(define-key gptel-prefix-map (kbd "C") (cons "AI [C]hat" 'gptel))

(global-set-key (kbd "C-c cf") 'eglot-format)
(global-set-key (kbd "C-c cc") 'compile)
(global-set-key (kbd "C-c ca") 'eglot-code-actions)
(global-set-key (kbd "M-[") 'flymake-goto-prev-error)
(global-set-key (kbd "M-]") 'flymake-goto-next-error)

(global-set-key (kbd "C-x R") 'hledger-run-command)

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(provide 'home-keybinds)
