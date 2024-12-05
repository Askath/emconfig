;; Ensure local packages are added to load path for compilation
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(eval-when-compile
  (require 'use-package))

;; Built-in packages
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Local packages
(eval-and-compile
  (require 'casual-suite))
(use-package casual-suite :ensure nil)

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

;; Utility packages
(use-package which-key
  :ensure nil
  :config
  (which-key-mode))

(use-package expand-region
  :ensure nil
  :commands er/expand-region
  :bind (("C-=" . er/expand-region)))

(use-package exec-path-from-shell :ensure nil)

;; Markdown mode
(use-package markdown-mode
  :ensure nil
  :hook (markdown-mode . visual-line-mode))

;; Calendar and org-mode extensions
(use-package calfw :ensure nil)
(use-package calfw-org :ensure nil)
(use-package calfw-blocks :ensure t)

(use-package org-modern :ensure nil)
(use-package org-contacts :ensure nil)

(use-package org
  :hook ((org-mode . visual-line-mode))
  :bind (:map global-map)
  :config
  (require 'oc-csl) ; citation support
  (add-to-list 'org-export-backends 'md)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-export-with-smart-quotes t))

(use-package org-super-agenda :ensure nil)
(use-package toc-org
  :ensure nil
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh"))
(use-package org-cliplink :ensure nil)
(use-package org-upcoming-modeline
  :config
  (org-upcoming-modeline-mode))

;; Finance
(use-package hledger-mode :ensure nil)

;; Clojure development
(use-package clojure-mode
  :ensure nil)
(use-package cider :ensure nil)

;; Common Lisp development
(use-package sly :ensure nil)

;; Git integration
(use-package magit
  :ensure nil
  :bind (("C-x g" . magit-status)))
(use-package elfeed 
  :ensure nil)
;; Other utilities
(use-package embark :ensure nil)

(use-package eshell
  :ensure nil
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure nil
  :custom
  (eat-term-name "iterm")
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

(use-package gptel 
:ensure nil)

(use-package ef-themes
:ensure nil)

(let ((site-lisp-dir "~/.emacs.d/site-lisp"))
  (when (file-directory-p site-lisp-dir)
    (dolist (dir (directory-files-recursively site-lisp-dir "dir$"))
      (add-to-list 'Info-default-directory-list (file-name-directory dir)))))

(when ( > emacs-major-version 30)
  (error "This Emacs config  only works with Emacs 30 and newer; you have version %s" emacs-major-version))

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

    ;;Disable line numbers
    (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
    (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

    (recentf-mode 1)

(setq custom-theme 'modus-operandi-tinted)

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


(require 'spacious-padding)

;; These are the default values, but I keep them here for visibility.
(setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))

(spacious-padding-mode 1)

;; Set a key binding if you need to toggle spacious padding.
(define-key global-map (kbd "<f8>") #'spacious-padding-mode)

(setopt sentence-end-double-space nil)

(setq xref-search-program 'ripgrep)
(setq grep-command "rg -nS --noheading")

(savehist-mode)

(windmove-default-keybindings 'control)

(when (display-graphic-p)
  (context-menu-mode))

(setq imenu-flatten 'prefix)

(defun bedrock--backup-file-name (fpath)
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath))
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)



(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
(setopt tab-always-indent 'complete)
(setopt completion-styles '(basic initials substring))
(setopt completion-auto-help 'always)
(setopt completions-max-height 20)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)
(ido-mode 0)
(fido-vertical-mode 1)
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

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

(which-key-add-key-based-replacements "C-x 3" "Open Split")
(which-key-add-key-based-replacements "C-x 1" "Close all but this")
(which-key-add-key-based-replacements "C-x 1" "Close this")

(which-key-add-key-based-replacements "C-x C-f" "[F]ile")
(define-prefix-command 'file-prefix-map)
(global-set-key (kbd "C-x C-f") 'file-prefix-map)
(define-key file-prefix-map (kbd "f") 'find-file)           ; Open file
(define-key file-prefix-map (kbd "r") 'recentf)            ; Open recent files

(global-set-key (kbd "C-=") (cons "Expand Selection" 'er/expand-region))
(global-set-key (kbd "s-v") 'cua-paste)
(global-set-key (kbd "C-v") 'scroll-up-command)
(global-set-key (kbd "M-s g") 'grep-find)
(global-set-key (kbd "C-x ,") 'ibuffer)
(global-set-key (kbd "C-c a") 'embark-act)
(global-set-key (kbd "M-i") 'imenu)

(which-key-add-key-based-replacements "C-c m" "[M]ajor mode")
(which-key-add-key-based-replacements "C-c o" "[O]pen")
(which-key-add-key-based-replacements "C-c c" "[C]ode")
(which-key-add-key-based-replacements "C-c j" "[J]ump to")
(which-key-add-key-based-replacements "C-c l" "[L]edger")
(which-key-add-key-based-replacements "C-c n" "[N]otes")
(which-key-add-key-based-replacements "C-c x" "[X] Capture")

(define-prefix-command 'gptel-prefix-map)
(global-set-key (kbd "C-c A") 'gptel-prefix-map)
;; (which-key-add-key-based-replacements "C-c A" "[A]I Chat")
(define-key gptel-prefix-map (kbd "m") (cons "Open AI [M]enu" 'gptel-menu))
(define-key gptel-prefix-map (kbd "s") (cons "[S]end to AI" 'gptel-send))
(define-key gptel-prefix-map (kbd "C") (cons "AI [C]hat" 'gptel))

(global-set-key (kbd "C-c cf") 'eglot-format)
(global-set-key (kbd "C-c cc") 'compile)
(global-set-key (kbd "C-c ca") 'eglot-code-actions)
(global-set-key (kbd "M-[") 'flymake-goto-prev-error)
(global-set-key (kbd "M-]") 'flymake-goto-next-error)

(global-set-key (kbd "C-x R") 'hledger-run-command)

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; Calendar showing org-agenda entries
(defun my-open-calendar-agenda ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "medium purple"))
   :view 'block-week))


(require 'calfw)
(require 'calfw-org)
(defun my-calfw-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources (list (cfw:org-create-source "Green"))
   :view 'block-week))


(defun my-calfw-set-view (view)
  (interactive)
  (setq cfw:current-view view)
  (cfw:refresh-calendar-buffer nil))

(define-key cfw:calendar-mode-map (kbd "M") (lambda () (interactive) (my-calfw-set-view 'month)))
(define-key cfw:calendar-mode-map (kbd "W") (lambda () (interactive) (my-calfw-set-view 'week)))
(define-key cfw:calendar-mode-map (kbd "D") (lambda () (interactive) (my-calfw-set-view 'day)))
(define-key cfw:calendar-mode-map (kbd "B") (lambda () (interactive) (my-calfw-set-view 'block-week)))
(define-key cfw:calendar-mode-map (kbd "1") (lambda () (interactive) (my-calfw-set-view 'block-day)))
(define-key cfw:calendar-mode-map (kbd "2") (lambda () (interactive) (my-calfw-set-view 'block-2-day)))
(define-key cfw:calendar-mode-map (kbd "3") (lambda () (interactive) (my-calfw-set-view 'block-3-day)))
(define-key cfw:calendar-mode-map (kbd "4") (lambda () (interactive) (my-calfw-set-view 'block-4-day)))
(define-key cfw:calendar-mode-map (kbd "5") (lambda () (interactive) (my-calfw-set-view 'block-5-day)))

(setq calfw-blocks-default-event-length 0.25)

  (define-prefix-command 'calendar-prefix-map)
  (which-key-add-key-based-replacements "C-c oc" "[C]alendar")
  (which-key-add-key-based-replacements "C-c c" "[C]ode")
  (global-set-key (kbd "C-c ns") 'org-search-view)
  (global-set-key (kbd "C-c na") 'org-agenda)
  (global-set-key (kbd "C-c x") 'org-capture)
  (global-set-key (kbd "C-c ocm") 'my-calfw-open-calendar)

 (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
  (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)
  (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
  (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
  (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
  (keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
  (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
  (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
  (keymap-global-set "M-g" #'casual-avy-tmenu)
  (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)
  (keymap-global-set "C-o" #'casual-editkit-main-tmenu)

(setq lombok-library-path (concat user-emacs-directory "lombok.jar"))
(unless (file-exists-p lombok-library-path)
  (url-copy-file "https://projectlombok.org/downloads/lombok.jar" lombok-library-path))

(use-package eglot
  ;; no :ensure nil here because it's built-in

  ;; Configure hooks to automatically turn on eglot for selected modes
  :hook
  ((js-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (java-ts-mode . eglot-ensure)
   (html-ts-mode . eglot-ensure)
   (clojure-mode . eglot-ensure)
   (clojurescript-mode . eglot-ensure)
   )

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event
  (add-to-list 'eglot-server-programs
               `(java-ts-mode . ("jdtls" "-data" "~/jdtls"
                                 "-javaagent:" ,lombok-library-path
                                 "-Xbootclasspath/a:" ,lombok-library-path
                                 "--jvm-arg=-XX:+UseG1GC"
                                 "--jvm-arg=-XX:+UseStringDeduplication"
                                 "-Djava.format.settings.url=file:///home/user/code-format.xml"
                                 "-Djava.format.settings.profile=myown")))
  (add-to-list 'eglot-server-programs
               `(html-ts-mode . ("node" "/opt/homebrew/lib/node_modules/@angular/language-server/" "--ngProbeLocations"
                                 "/opt/homebrew/lib/node_modules" "--tsProbeLocations"
                                 "/opt/homebrew/lib/node_modules/" "--stdio"))))


(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))



(with-eval-after-load 'org (global-org-modern-mode))

;; Agenda variables
(setq org-directory "~/org/") ; Non-absolute paths for agenda and

(setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))


(setq org-startup-folded 'fold)

(setq org-contacts-vcard-file "contacts.vcard")


(setq org-capture-templates
      '(("a" "Appointment" entry (file+headline "~/org/todos.org" "schedules")
         "* %?\n  %i\n")
        ("t" "Todo Private" entry (file+headline "~/org/todos.org" "Private")
         "** TODO %? :private:\n  %i\n")
        ("T" "Todo Work" entry (file+headline "~/org/todos.org" "Work")
         "** TODO %? :work:\n  %i\n")
        ("r" "Recurring" entry (file "~/org/recurring_calendar.org")
         "** TODO  %?\n "
         )
        ("l" "Link" entry (file+headline "~/org/link.org" "to watch/read")
         "** \n"
         )
        ("n" "Note" entry (file "~/org/inbox.org")
         "* %?"
         )
        ("m" "Meeting" entry (file "~/org/notes/areas/work/meetings/notes.org")
         "* %? "
         )
        ("c" "Contacts" entry (file "~/org/contacts.org")
         "* %(org-contacts-template-name)
       :PROPERTIES:
       :EMAIL: %(org-contacts-template-email)
       :PHONE:
       :ALIAS:
       :NICKNAME:
       :IGNORE:
       :ICON:
       :NOTE:
       :ADDRESS:
       :BIRTHDAY:
       :END:"))
      )

(setq org-contacts-files (list "~/org/contacts/contacts.org"))
(setq org-contacts-vcard-file "contacts.vcard")



(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil ;; i.e. today
        org-agenda-span 7
        org-agenda-start-on-weekday nil)
  (setq org-deadline-warning-days 90)
  (setq org-agenda-warning-days 90)
  (setq org-agenda-deadline-leaders '("Due: " "Due in %d days: " "Overdue %d days ago: "))
  (setq org-agenda-scheduled-leaders '("Scheduled: " "Scheduled in %d days: " "Scheduled %d days ago: "))
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-custom-commands
        '(
          ("c" "Today"
           ((agenda "" (
                        (org-agenda-span 1)
                        (org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "NOTES"
                                  :tag "marker"
                                  :order 1
                                  :transformer (--> it
                                                    (upcase it)
                                                    (propertize it 'face 
                                                                '(:background "goldenrod3" :foreground "black"))))
                           (:name "Today"
                                  :time-grid t
                                  :scheduled today
                                  :order 2)
                           (:name "Recurring"
                                  :tag "recurring" 
                                  :order 3)
                           (:name "Upcoming schedules"
                                  :tag "appointment"
                                  :deadline t
                                  :time-grid t
                                  :order 4)
                           (:name "Morning Routine"
                                  :tag "morning_routine"
                                  :order 5)
                           (:discard (:scheduled future :deadline t :tag "idle" :anything t))
                           ))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '(
                            (:name "Clocks"
                                   :tag "clock"
                                   :date today
                                   :order 1)
                            (:name "NEXT"
                                   :todo "NEXT"
                                   :order 2
                                   :transformer (--> it 
                                                     (propertize it 'face '(:background "blue"))))
                            (:name "Private"
                                   :and (:tag "private" :deadline nil)
                                   :order 3)
                            (:name "Work"
                                   :tag "work"
                                   :order 4)
                            (:discard (:tag "recurring" :scheduled t :tag "appointment" :deadline t :tag "morning_routine" :tag "evening_routine"))
                            )))
                     ))
           ))))
(org-super-agenda-mode)



;; Default t
(setq org-todo-keywords
      '((sequence
         "TODO(t)"  ; A task that needs doing & is ready to do
         "NEXT(n)"
         "PROJ(p)"  ; A project, which usually contains other tasks
         "RESC(r)"
         "PROGR(s)"  ; A task that is in progress
         "WAIT(w)"  ; Something external is holding up this task
         "HOLD(h)"  ; This task is paused/on hold because of me
         "IDEA(i)"  ; An unconfirmed and unapproved task or notion
         "|"
         "DONE(d)"  ; Task successfully completed
         "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
        (sequence
         "[ ](T)"   ; A task that needs doing
         "[-](S)"   ; Task is in progress
         "[?](W)"   ; Task is being held up or paused
         "|"
         "[X](D)")  ; Task was completed
        )
      org-todo-keyword-faces
      '(("[-]"  . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("[?]"  . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)
        ("KILL" . +org-todo-cancel)))



(global-set-key (kbd "C-x p i") 'org-cliplink)
(global-set-key (kbd "C-c C") 'my-org-clock-in-default-task)

(require 'org-clock)
(defun org-clock-todo-change ()
  (cond
   ((string= org-state "PROGR")
    (org-clock-in))
   ((string= org-state "DONE")
    (unless (org-clocking-p)
      (org-clock-in))
    (org-clock-out))
   (t
    (when (org-clocking-p)
      (org-clock-out)))))

(add-hook 'org-after-todo-state-change-hook 'org-clock-todo-change)

(require 'hledger-mode)
;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . hledger-mode))
;; Provide the path to you journal file.
;; The default location is too opinionated.
(setq hledger-jfile "~/org/finance/2024.ledger")
(global-set-key (kbd "C-c C-l e") 'hledger-jentry)
(global-set-key (kbd "C-c C-l l") 'hledger-run-command)
(setq hledger-currency-string "EUR"
      hledger-extra-args ""
      hledger-extrapolate-savings-period 12
      hledger-extrapolate-savings-rate 7.0
      hledger-ratios-assets-accounts "Assets"
      hledger-ratios-debt-accounts "Liabilities"
      hledger-ratios-essential-expense-accounts ""
      hledger-ratios-income-accounts
      "Revenue:Gehalt Revenue:Sonstiges Revenue:Netflix Revenue:Refunds"
      hledger-ratios-liquid-asset-accounts "Assets:Cash Assets:Checking Assets:Savings"
      hledger-show-expanded-report t
      hledger-top-asset-account "Assets"
      hledger-top-expense-account "Expenses"
      hledger-top-income-account "Revenue"
      hledger-year-of-birth 1997)

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))



(use-package emacs
  :config
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (html-mode . html-ts-mode)
          (mhtml-mode . html-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ((prog-mode . electric-pair-mode)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter-tree-sitter-css")
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

(defun vendor/open-iterm2 ()
  "Open Puffin in a new iTerm window."
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (do-applescript
     (format
      "tell application \"iTerm\"
        create window with default profile
        tell current session of current window
          write text \"cd %s\"
        end tell
      end tell" dir))))

(global-set-key (kbd "C-c ot") 'vendor/open-iterm2)

(defun org-download-work-calendar (link output)
  (let ((command (list "wget" link "-O" output)))
    (with-temp-buffer
      (let ((exit-code (apply 'call-process (car command) nil t nil (cdr command))))
        (if (= exit-code 0)
            (message "Downloaded calendar")
          (message "Failed to download calendar with exit code %d" exit-code))))))

(defun org-convert-work-calendar (input output)
  (let ((command (list "ical2orgpy" input output)))
    (with-temp-buffer
      (let ((exit-code (apply 'call-process (car command) nil t nil (cdr command))))
        (if (= exit-code 0)
            (message "Converted calendar")
          (message "Failed to convert calendar with exit code %d" exit-code)))
      (let ((command (list "rm" "-rf" input)))
        (apply 'call-process (car command) nil t nil (cdr command))
        )
      )))

(defun org-refresh-work-calendar ()
  (interactive)
  (org-download-work-calendar "https://outlook.office365.com/owa/calendar/5a510932ff8a4fd6b4d73fa1203d4683@optadata.de/f18ceea7bff54438ab2e6fd922b669bc5942294383520341840/calendar.ics"
                              (expand-file-name "~/org/calendar_work.ical"))
  (org-convert-work-calendar (expand-file-name "~/org/calendar_work.ical")
                             (expand-file-name "~/org/calendar_work.org")))



(defun vpn/status ()
  (interactive)
x  (let ((output-buffer (get-buffer-create "*VPN Status*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (let ((exit (call-process "/System/Volumes/Data/opt/cisco/secureclient/bin/vpn" nil t nil "-s" "status")))
        (if (= exit 0)
            (insert "VPN connection status: worked\n")
          (insert "VPN connection status: failed\n"))))
    (pop-to-buffer output-buffer)))


(defun vpn/disconnect ()
  (interactive)
  (let ((output-buffer (get-buffer-create "*VPN Status*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (let ((exit (call-process "/System/Volumes/Data/opt/cisco/secureclient/bin/vpn" nil t nil "-s" "disconnect")))
        (if (= exit 0)
            (insert "VPN connection status: worked\n")
          (insert "VPN connection status: failed\n"))))
    (pop-to-buffer output-buffer)))

(defun vpn/connect ()
  (interactive)
  (let ((output-buffer (get-buffer-create "*VPN Status*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (let ((exit (call-process "/Users/taradruffel/.workspace/scripts/connect_vpn.clj" nil t nil)))
        (if (= exit 0)
            (insert "VPN connection status: worked\n")
          (insert "VPN connection status: failed\n"))))
    (pop-to-buffer output-buffer)))

(setq gptel-default-mode 'text-mode)
(setq gptel-api-key "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")

;; keybinds for chat gpt in emacs with C-c c as prefix

(global-set-key (kbd "C-x w") 'elfeed)


;; Somewhere in your .emacs file
(setq elfeed-feeds
      '(
        "https://planet.emacslife.com/atom.xml"
        )
)

(setq org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))

(setq prefix-help-command #'embark-prefix-help-command)
