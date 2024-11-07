(set-frame-font "Monaspace Neon Var-16" nil t)

(delete-selection-mode 1)


;; Enable line numbers globally
(global-display-line-numbers-mode)

;; Disable line numbers in a specific mode (e.g., org-mode)
(add-hook 'org-timeblock-mode-hook (lambda () (display-line-numbers-mode 0)))

;; Enable line numbers again when entering another mode, if needed
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

(recentf-mode 1)
(cua-mode -1)
;; Package setup
(use-package expand-region
  :ensure t)

(require 'expand-region)

(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq which-key-sort-order 'which-key-key-order-alpha)
(setq which-key-max-display-columns 2)
(setq which-key-popup-type 'side-window)
(setq which-key-side-window-location 'right)
(setq which-key-side-window-max-height 0.25)
(setq which-key-side-window-max-width 0.33)
(which-key-add-key-based-replacements "C-x 3" "Open Split")

(which-key-add-key-based-replacements "C-x C-f" "[F]ile")
(define-prefix-command 'file-prefix-map)
(global-set-key (kbd "C-x C-f") 'file-prefix-map)
(define-key file-prefix-map (kbd "f") 'find-file)           ; Open file
(define-key file-prefix-map (kbd "r") 'recentf)  ; Open recent files

;; Level 1 Keys (C-c)
(global-set-key (kbd "C-=") (cons "Expand Selection" 'er/expand-region))
(global-set-key (kbd "s-v") 'cua-paste)
(global-set-key (kbd "C-v") 'scroll-up-command)

;; Level 2 keys
(which-key-add-key-based-replacements "C-c o" "[O]pen")
(which-key-add-key-based-replacements "C-c c" "[C]code")
(which-key-add-key-based-replacements "C-c j" "[J]ump to")
(which-key-add-key-based-replacements "C-c l" "[L]edger")
(which-key-add-key-based-replacements "C-c n" "[N]otes")
(which-key-add-key-based-replacements "C-c x" "[X] Capture")

;; GPT/AI Keybinds
(define-prefix-command 'gptel-prefix-map)
(global-set-key (kbd "C-c oc") 'gptel-prefix-map)
(which-key-add-key-based-replacements "C-c oc" "AI [C]hat")
(define-key gptel-prefix-map (kbd "m") (cons "Open AI [M]enu" 'gptel-menu))
(define-key gptel-prefix-map (kbd "s") (cons "[S]end to AI" 'gptel-send))
(define-key gptel-prefix-map (kbd "C") (cons "AI [C]hat" 'gptel))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)


;; C-c c CODE-ACTIONS
(global-set-key (kbd "C-c cf") 'eglot-format)
(global-set-key (kbd "C-c cc") 'compile)
(global-set-key (kbd "C-c ca") 'eglot-code-actions)
    

(global-set-key (kbd "M-[") 'flymake-goto-prev-error)
(global-set-key (kbd "M-]") 'flymake-goto-next-error)



(global-set-key (kbd "C-c lr") 'hledger-run-command)
(global-set-key (kbd "C-c le") 'hledger-jentry)
;; Disable indent on ENTER
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))


(global-set-key (kbd "C-c ns") 'org-search-view)
(global-set-key (kbd "C-c ncd") 'org-timeblock)
(global-set-key (kbd "C-c ncm") 'cfw:open-org-calendar)
(global-set-key (kbd "C-c na") 'org-agenda)
(global-set-key (kbd "C-c x") 'org-capture)
(global-set-key (kbd "C-c nf") 'ido-find-file-in-dir)
(global-set-key (kbd "C-c n f") (lambda () (interactive) (consult-find "~/org")))
