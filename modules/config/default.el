(set-frame-font "Monaspace Neon Var-16" nil t)

(recentf-mode 1)
(cua-mode -1)
;; Package setup
(use-package expand-region
  :ensure t)

(require 'expand-region)

(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "s-v") 'cua-paste)
(global-set-key (kbd "C-v") 'scroll-up-command)
(define-prefix-command 'file-prefix-map)

;; C-c c CODE-ACTIONS
(global-set-key (kbd "C-c cf") 'eglot-format)
(global-set-key (kbd "C-c cc") 'compile)
(global-set-key (kbd "C-c ca") 'eglot-code-actions)
    

;; C-x Prefix FIND-FILE
(global-set-key (kbd "C-x C-f") 'file-prefix-map)
(define-key file-prefix-map (kbd "f") 'find-file)           ; Open file
(define-key file-prefix-map (kbd "r") 'recentf)  ; Open recent files

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Disable indent on ENTER
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))


