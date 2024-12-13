;; Package management and configuration
(when ( > emacs-major-version 30)
  (error "This Emacs config only works with Emacs 30 and newer; you have version %s" emacs-major-version))

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(require 'use-package)
(setq use-package-always-ensure t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(load-file "~/.emacs.d/completion.el")
(load-file "~/.emacs.d/filebrowser.el")

;; Built-in packages
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Helpful Packages
(use-package casual-suite :ensure t)
(use-package helpful :ensure t)

(use-package which-key
  :ensure nil
  :config
  (which-key-mode))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind (("C-=" . er/expand-region)))

(use-package exec-path-from-shell :ensure t)

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . visual-line-mode))

(use-package calfw :ensure t)
(use-package calfw-org :ensure t)
(use-package calfw-blocks :ensure nil)

(use-package org-modern :ensure t)

(use-package org-super-agenda :ensure t)
(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh"))
(use-package org-cliplink :ensure t)
(use-package org-upcoming-modeline
  :ensure t
  :config
  (org-upcoming-modeline-mode))

(use-package hledger-mode :ensure nil)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package elfeed 
  :ensure t)

(use-package eshell
  :ensure t
  :init
  (defun bedrock/setup-eshell ()
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

(use-package eat
  :ensure t
  :custom
  (eat-term-name "iterm")
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode))

(use-package gptel 
  :ensure t)

(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))

(use-package paredit :ensure t)

(use-package aider
  :vc (:url "http://github.com/tninja/aider.el" :rev :newest)
  :config
  (setq aider-args '("--model" "gpt-4o-mini")))

(let ((site-lisp-dir "~/.emacs.d/site-lisp"))
  (when (file-directory-p site-lisp-dir)
    (dolist (dir (directory-files-recursively site-lisp-dir "dir$"))
      (add-to-list 'Info-default-directory-list (file-name-directory dir)))))

(setq custom-theme 'modus-vivendi-tinted)
