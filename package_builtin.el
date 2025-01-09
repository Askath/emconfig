(use-package doom-themes
  :ensure nil
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is univrsally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Finance
(use-package hledger-mode :ensure nil)

(use-package delsel
  :hook (after-init . delete-selection-mode) :ensure nil)

(use-package exec-path-from-shell
  :ensure nil
  :config
  (exec-path-from-shell-initialize))

(use-package spacious-padding :ensure nil
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))
  )

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-headings '((1 . (1.5))
                           (2 . (1.3))
                           (t . (1.1))))
  (modus-themes-to-toggle
   '(modus-operandi-tinted modus-vivendi-tinted))
  :bind (("C-c w m" . modus-themes-toggle)
         ("C-c w M" . modus-themes-select)))

;; Utility packages
(use-package which-key
  :ensure nil
  :config
  (which-key-mode))

(use-package expand-region
  :ensure nil
  :commands er/expand-region
  :bind (("C-=" . er/expand-region)))

;; Markdown mode
(use-package markdown-mode
  :ensure nil
  :hook (markdown-mode . visual-line-mode))

;; Calendar and org-mode extensions
(use-package calfw :ensure nil)
(use-package calfw-org :ensure nil)
(use-package calfw-blocks :ensure nil)

(use-package org
  :hook ((org-mode . visual-line-mode))
  :bind (:map global-map)
  :config
  (require 'oc-csl) ; citation support
  (add-to-list 'org-export-backends 'md)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-export-with-smart-quotes t))
