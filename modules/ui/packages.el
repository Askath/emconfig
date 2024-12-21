;; Theme configuration
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

(use-package spacious-padding
  :ensure t
  :config
    (spacious-padding-mode 1)
)
