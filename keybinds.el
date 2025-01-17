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
(define-key gptel-prefix-map (kbd "r") (cons "[R]efactor" 'gptel-rewrite))

(global-set-key (kbd "C-c c r") 'eglot-rename)
(global-set-key (kbd "C-c c a") 'eglot-code-actions)
(global-set-key (kbd "C-c c f") 'eglot-format)
(global-set-key (kbd "C-c c i") 'eglot-code-action-organize-imports)
(global-set-key (kbd "C-c c I") 'eglot-code-action-unused-imports-ts)
(global-set-key (kbd "C-c c t") 'eglot-find-typeDefinition)
(global-set-key (kbd "C-c c e") 'eglot-reconnect)
(global-set-key (kbd "C-c c q") 'eglot-shutdown)
(global-set-key (kbd "C-c c s") 'eglot)
(global-set-key (kbd "C-x A") 'aider-transient-menu)

(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "M-[") 'flymake-goto-prev-error)
(global-set-key (kbd "M-]") 'flymake-goto-next-error)

(global-set-key (kbd "C-x R") 'hledger-run-command)

(define-prefix-command 'calendar-prefix-map)
(which-key-add-key-based-replacements "C-c oc" "[C]alendar")
(which-key-add-key-based-replacements "C-c c" "[C]ode")
(global-set-key (kbd "C-c ns") 'org-search-view)
(global-set-key (kbd "C-c na") 'org-agenda)
(global-set-key (kbd "C-c x") 'org-capture)
(global-set-key (kbd "<f6>") 'cfw:open-org-calendar)
(global-set-key (kbd "C-c oc") 'cfw:open-org-calendar)

(global-set-key (kbd "C-x p i") 'org-cliplink)
(global-set-key (kbd "C-c C") 'my-org-clock-in-default-task)
