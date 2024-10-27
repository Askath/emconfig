(set-frame-font "Monaspace Neon Var-16" nil t)

(cua-mode -1)
;; Package setup
(use-package expand-region
  :ensure t)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(global-set-key (kbd "s-v") 'cua-paste)
(global-set-key (kbd "C-v") 'scroll-up-command)
(global-set-key (kbd "C-c ff") 'find-file)
(global-set-key (kbd "C-c fr") 'recentf)
