;; User interface settings
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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

(spacious-padding-mode 1)

(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'fundamental-mode)
(setopt display-time-default-load-average nil)
(setq make-backup-files nil)
(setq global-auto-revert-mode 1)
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(delete-selection-mode 1)

(global-display-line-numbers-mode)

;; Disable line numbers in org-mode
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

(recentf-mode 1)
(setq make-backup-files nil)

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
                        :family "Iosevka SS08"
                        :height 180)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'set-default-font)
  (set-default-font (selected-frame)))

(setopt sentence-end-double-space nil)

(setq xref-search-program 'ripgrep)
(setq grep-command "rg -nS --noheading")

(savehist-mode)

(windmove-default-keybindings 'control)

(when (display-graphic-p)
  (context-menu-mode))

(setq imenu-flatten 'prefix)

(setopt line-number-mode t)
(setopt column-number-mode t)
(setq display-line-numbers-type 'relative)
(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)
(setopt show-trailing-whitespace nil)
(setopt indicate-buffer-boundaries 'left)
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)
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
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(setq which-key-sort-order 'which-key-key-order-alpha)
(setq which-key-max-display-columns 2)
(setq which-key-popup-type 'side-window)
(setq which-key-side-window-location 'right)
(setq which-key-side-window-max-height 0.25)
(setq which-key-side-window-max-width 0.33)
