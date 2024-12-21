(load "./packages.el")

(setq custom-theme 'modus-vivendi-tinted)

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
                            :height 180
                            )))         ; Adjust the height as needed

    (if (daemonp)
        (add-hook 'after-make-frame-functions #'set-default-font)
      (set-default-font (selected-frame)))



      ;; Distraction-free writing
      (defun ews-distraction-free ()
        "Distraction-free writing environment using Olivetti package."
        (interactive)
        (if (equal olivetti-mode nil)
            (progn
              (window-configuration-to-register 1)
              (delete-other-windows)
              (text-scale-set 1)
              (olivetti-mode t))
          (progn
            (if (eq (length (window-list)) 1)
                (jump-to-register 1))
            (olivetti-mode 0)
            (text-scale-set 0))))


  ;; These are the default values, but I keep them here for visibility.
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))


  (when (display-graphic-p)
    (context-menu-mode))

(setq imenu-flatten 'prefix)

  ;; Set a key binding if you need to toggle spacious padding.
  (define-key global-map (kbd "<f8>") #'spacious-padding-mode)

(setopt line-number-mode t)
(setopt column-number-mode t)
(global-display-line-numbers-mode)
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


;; (when (memq window-system '(mac ns x))
 ;; (exec-path-from-shell-initialize))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(use-package which-key
  :ensure nil
  :config
  (which-key-mode))

(which-key-mode 1)
(provide 'home-ui)
