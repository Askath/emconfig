(use-package calfw :ensure t)
(use-package calfw-org :ensure t)
(use-package calfw-blocks :ensure nil)

(use-package org-modern :ensure t)
(use-package org-contacts :ensure t)

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode))
  :bind (:map global-map)
  :config
  (require 'oc-csl) ; citation support
  (add-to-list 'org-export-backends 'md)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-export-with-smart-quotes t))

(use-package org-super-agenda :ensure t)
(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh"))
(use-package  org-cliplink :ensure t)
(use-package org-upcoming-modeline
  :ensure t
  :config
  (org-upcoming-modeline-mode))

