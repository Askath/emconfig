;;;  ________                                                _______                 __                            __
;;; /        |                                              /       \               /  |                          /  |
;;; $$$$$$$$/ _____  ____   ______   _______  _______       $$$$$$$  | ______   ____$$ | ______   ______   _______$$ |   __
;;; $$ |__   /     \/    \ /      \ /       |/       |      $$ |__$$ |/      \ /    $$ |/      \ /      \ /       $$ |  /  |
;;; $$    |  $$$$$$ $$$$  |$$$$$$  /$$$$$$$//$$$$$$$/       $$    $$</$$$$$$  /$$$$$$$ /$$$$$$  /$$$$$$  /$$$$$$$/$$ |_/$$/
;;; $$$$$/   $$ | $$ | $$ |/    $$ $$ |     $$      \       $$$$$$$  $$    $$ $$ |  $$ $$ |  $$/$$ |  $$ $$ |     $$   $$<
;;; $$ |_____$$ | $$ | $$ /$$$$$$$ $$ \_____ $$$$$$  |      $$ |__$$ $$$$$$$$/$$ \__$$ $$ |     $$ \__$$ $$ \_____$$$$$$  \
;;; $$       $$ | $$ | $$ $$    $$ $$       /     $$/       $$    $$/$$       $$    $$ $$ |     $$    $$/$$       $$ | $$  |
;;; $$$$$$$$/$$/  $$/  $$/ $$$$$$$/ $$$$$$$/$$$$$$$/        $$$$$$$/  $$$$$$$/ $$$$$$$/$$/       $$$$$$/  $$$$$$$/$$/   $$/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings for quick startup and convenience
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent
      inhibit-startup-echo-area-message (user-login-name)
      frame-resize-pixelwise t
      default-frame-alist '((fullscreen . maximized)
                            (background-color . "#000000")
                            (foreground-color . "#ffffff")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name)
      inhibit-splash-screen t
      initial-major-mode 'fundamental-mode
      display-time-default-load-average nil
      make-backup-files nil
      global-auto-revert-mode 1
      auto-revert-avoid-polling t
      auto-revert-interval 5
      auto-revert-check-vc-info t
      line-number-mode t
      column-number-mode t
      display-line-numbers-type 'relative
      x-underline-at-descent-line nil
      switch-to-buffer-obey-display-actions t
      show-trailing-whitespace nil
      indicate-buffer-boundaries 'left
      mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t
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

(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway
(setq default-frame-alist '((fullscreen . maximized)

                            ;; You can turn off scroll bars by uncommenting these lines:
                            ;; (vertical-scroll-bars . nil)
                            ;; (horizontal-scroll-bars . nil)

                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#000000")
                            (foreground-color . "#ffffff")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))
