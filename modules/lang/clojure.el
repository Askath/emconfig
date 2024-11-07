(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

(use-package cider
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
