(use-package devdocs
  :ensure t
  :init
  :bind (
	     :map devdocs-mode-map (
				                "C-h D" . devdocs-lookup
				))
  )
