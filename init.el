;; Ensure local packages are added to load path for compilation
(when ( > emacs-major-version 30)
  (error "This Emacs config  only works with Emacs 30 and newer; you have version %s" emacs-major-version))

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(let ((site-lisp-dir "~/.emacs.d/site-lisp"))
  (when (file-directory-p site-lisp-dir)
    (dolist (dir (directory-files-recursively site-lisp-dir "dir$"))
      (add-to-list 'Info-default-directory-list (file-name-directory dir)))))
(add-to-list 'load-path "~/.emacs.d/site-lisp/vertico")
(add-to-list 'load-path "~/.emacs.d/site-lisp/orderless")
(add-to-list 'load-path "~/.emacs.d/site-lisp/embark")
(add-to-list 'load-path "~/.emacs.d/site-lisp/marginalia")
(add-to-list 'load-path "~/.emacs.d/site-lisp/corfu")
(add-to-list 'load-path "~/.emacs.d/site-lisp/dirvish")
(add-to-list 'load-path "~/.emacs.d/site-lisp/exec-path-from-shell")
(add-to-list 'load-path "~/.emacs.d/site-lisp/spacious-padding")
(add-to-list 'load-path "~/.emacs.d/site-lisp/modus-themes")
(add-to-list 'load-path "~/.emacs.d/site-lisp/which-key")
(add-to-list 'load-path "~/.emacs.d/site-lisp/expand-region")
(add-to-list 'load-path "~/.emacs.d/site-lisp/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/calfw")
(add-to-list 'load-path "~/.emacs.d/site-lisp/calfw-org")
(add-to-list 'load-path "~/.emacs.d/site-lisp/calfw-blocks")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-super-agenda")
(add-to-list 'load-path "~/.emacs.d/site-lisp/toc-org")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-cliplink")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-upcoming-modeline")
(add-to-list 'load-path "~/.emacs.d/site-lisp/aider")
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit")
(add-to-list 'load-path "~/.emacs.d/site-lisp/eshell")
(add-to-list 'load-path "~/.emacs.d/site-lisp/eat")
(add-to-list 'load-path "~/.emacs.d/site-lisp/gptel")
(add-to-list 'load-path "~/.emacs.d/site-lisp/dabbrev")
(add-to-list 'load-path "~/.emacs.d/site-lisp/paredit")
(add-to-list 'load-path "~/.emacs.d/site-lisp/helpful")
(load-file "~/.emacs.d/packages.el")
(require 'doom-themes)
(setq custom-theme 'doom-Iosvkem)

(defun apply-modus-operandi-tinted-palette (frame)
  (with-selected-frame frame
    (load-theme custom-theme t)))
(if (daemonp)
    (add-hook 'after-make-frame-functions #'apply-modus-operandi-tinted-palette)
  (load-theme custom-theme t))

(defun set-default-font (frame)
  "Set the default font for the FRAME."
  (with-selected-frame frame
    (set-face-attribute 'default nil
                        :family "Iosevka SS08" ; Replace with your font name
                        :height 160
                        )))         ; Adjust the height as needed

(if (daemonp)
    (add-hook 'after-make-frame-functions #'set-default-font)
  (set-default-font (selected-frame)))

(load-file "~/.emacs.d/keybinds.el")
(load-file "~/.emacs.d/custom_functions.el")

(require 'custom-functions)

;; disable auto save mode
(auto-save-mode -1)

;; THEMES AND UI ;;
(delete-selection-mode 1)
(global-display-line-numbers-mode)

;;Disable line numbers
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
(savehist-mode)
(setopt sentence-end-double-space nil)

;; OTHER SETTINGS ;;
;;change grep command to rg
(setq xref-search-program 'ripgrep
      grep-command "rg -nS --no-heading "
      grep-find-template "find . -type f -exec rg -nS --no-heading \\{\\} + ")

(when (display-graphic-p)
  (context-menu-mode))

(setq imenu-flatten 'prefix
      line-number-mode t
      column-number-mode t
      display-line-numbers-type 'relative
      x-underline-at-descent-line nil
      switch-to-buffer-obey-display-actions t
      show-trailing-whitespace nil
      indicate-buffer-boundaries 'left
      mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t
      display-line-numbers-width 3
      tab-bar-show 1
      display-time-format "%a %F %T"
      display-time-interval 1
      indent-tabs-mode nil
      tab-width 4
      indent-line-function 'insert-tab
      which-key-sort-order 'which-key-key-order-alpha
      which-key-max-display-columns 2
      which-key-popup-type 'side-window
      which-key-side-window-location 'right
      which-key-side-window-max-height 0.25
      which-key-side-window-max-width 0.33)

(defun bedrock--backup-file-name (fpath)
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath))
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

(blink-cursor-mode -1)
(pixel-scroll-precision-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)
(add-hook 'text-mode-hook 'visual-line-mode)
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

(setopt tab-bar-show 1)
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(display-time-mode)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))



(require 'calfw)
(require 'calfw-org)
(require 'hledger-mode)
;; To open files with .journal extension in hledger-mode

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (zig "https://github.com/tree-sitter-grammars/tree-sitter-zig=")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (java "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure")))

(setq org-safe-remote-resources
      '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))

(setq prefix-help-command #'embark-prefix-help-command)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aider-args '("--model" "gpt-4o-mini"))
 '(custom-safe-themes
   '("b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1"
     "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     default))
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(package-selected-packages nil)
 '(package-vc-selected-packages '((aider :url "https://github.com/tninja/aider.el")))
 '(zig-format-on-save nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
