;; Custom keybindings
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
(global-set-key (kbd "C-h .") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)

(global-set-key (kbd "C-c Aa") 'aider-transient-menu)
(global-set-key (kbd "<f8>") #'spacious-padding-mode)

(global-set-key (kbd "C-x C-f") 'file-prefix-map)
(define-key file-prefix-map (kbd "f") 'find-file)
(define-key file-prefix-map (kbd "r") 'recentf)

(global-set-key (kbd "C-=") (cons "Expand Selection" 'er/expand-region))
(global-set-key (kbd "s-v") 'cua-paste)
(global-set-key (kbd "C-v") 'scroll-up-command)
(global-set-key (kbd "M-s g") 'grep-find)
(global-set-key (kbd "C-x ,") 'ibuffer)
(global-set-key (kbd "M-i") 'imenu)

(global-set-key (kbd "C-x R") 'hledger-run-command)

(global-set-key (kbd "C-c c r") 'eglot-rename)
(global-set-key (kbd "C-c c a") 'eglot-code-actions)
(global-set-key (kbd "C-c c f") 'eglot-format)
(global-set-key (kbd "C-c c i") 'eglot-code-action-organize-imports)
(global-set-key (kbd "C-c c I") 'eglot-code-action-unused-imports-ts)
(global-set-key (kbd "C-c c t") 'eglot-find-typeDefinition)
(global-set-key (kbd "C-c c e") 'eglot-reconnect)
(global-set-key (kbd "C-c c q") 'eglot-shutdown)
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "M-[") 'flymake-goto-prev-error)
(global-set-key (kbd "M-]") 'flymake-goto-next-error)

(global-set-key (kbd "C-c ns") 'org-search-view)
(global-set-key (kbd "C-c na") 'org-agenda)
(global-set-key (kbd "C-c x") 'org-capture)
(global-set-key (kbd "C-c ocm") 'my-calfw-open-calendar)

(define-prefix-command 'gptel-prefix-map)
(global-set-key (kbd "C-c A") 'gptel-prefix-map)
(define-key gptel-prefix-map (kbd "m") (cons "Open AI [M]enu" 'gptel-menu))
(define-key gptel-prefix-map (kbd "s") (cons "[S]end to AI" 'gptel-send))
(define-key gptel-prefix-map (kbd "C") (cons "AI [C]hat" 'gptel))
(define-key gptel-prefix-map (kbd "r") (cons "[R]efactor" 'gptel-rewrite))
