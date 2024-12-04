(require 'package)
(package-initialize)
(require 'org)

;; Add your package archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(org-babel-tangle-file "init.org")
(byte-recompile-directory "~/.emacs.d" 0 t)
;; Load your configuration file
(load-file "~/.emacs.d/init.el")

;; Download and compile all packages
(setq use-package-always-ensure t)

