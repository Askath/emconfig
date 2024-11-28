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

    (add-hook 'org-timeblock-mode-hook (lambda () (display-line-numbers-mode 0)))
    (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
    (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

    (recentf-mode 1)

    (add-to-list 'package-archives
                 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
    (package-initialize)

  (use-package casual-suite :ensure t)
  (require 'casual-suite)
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

(use-package delsel
  :ensure nil ; no need to install it as it is built-in
  :hook (after-init . delete-selection-mode))

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
      :bind
      (("C-c w m" . modus-themes-toggle)
       ("C-c w M" . modus-themes-select)))

  (use-package ef-themes :ensure t)
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
                          :family "Iosevka Aile" ; Replace with your font name
                          :height 160
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

    (use-package olivetti
      :ensure t
      :demand t
      :bind
      (("<f9>" . ews-distraction-free)))
(setq olivetti-body-width 90)
(setq olivetti-minium-Body-width 60)

(use-package spacious-padding :ensure t)
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

(defun bedrock--backup-file-name (fpath)
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath))
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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
(ido-mode 1)
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

(use-package expand-region
  :ensure t)
(require 'expand-region)

(use-package exec-path-from-shell :ensure t)
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

(which-key-add-key-based-replacements "C-c o" "[O]pen")
(which-key-add-key-based-replacements "C-c c" "[C]ode")
(which-key-add-key-based-replacements "C-c j" "[J]ump to")
(which-key-add-key-based-replacements "C-c l" "[L]edger")
(which-key-add-key-based-replacements "C-c n" "[N]otes")
(which-key-add-key-based-replacements "C-c x" "[X] Capture")

(define-prefix-command 'gptel-prefix-map)
(global-set-key (kbd "C-c oc") 'gptel-prefix-map)
(which-key-add-key-based-replacements "C-c oc" "AI [C]hat")
(define-key gptel-prefix-map (kbd "m") (cons "Open AI [M]enu" 'gptel-menu))
(define-key gptel-prefix-map (kbd "s") (cons "[S]end to AI" 'gptel-send))
(define-key gptel-prefix-map (kbd "C") (cons "AI [C]hat" 'gptel))

(global-set-key (kbd "C-c cf") 'eglot-format)
(global-set-key (kbd "C-c cc") 'compile)
(global-set-key (kbd "C-c ca") 'eglot-code-actions)
(global-set-key (kbd "M-[") 'flymake-goto-prev-error)
(global-set-key (kbd "M-]") 'flymake-goto-next-error)

(global-set-key (kbd "C-c lr") 'hledger-run-command)
(global-set-key (kbd "C-c le") 'hledger-jentry)

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(global-set-key (kbd "C-c ns") 'org-search-view)
(global-set-key (kbd "C-c ncd") 'org-timeblock)
(global-set-key (kbd "C-c ncm") 'cfw:open-org-calendar)
(global-set-key (kbd "C-c na") 'org-agenda)
(global-set-key (kbd "C-c x") 'org-capture)

(load-file (expand-file-name "modules/tools/eglot.el" user-emacs-directory))

(use-package markdown-mode
:ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package org-timeblock :ensure t)
(use-package calfw :ensure t)
(use-package calfw-org :ensure t)
(use-package org-modern :ensure t)

(with-eval-after-load 'org (global-org-modern-mode))

;; Agenda variables
(setq org-directory "~/org/") ; Non-absolute paths for agenda and

(setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))

(use-package org
  :hook ((org-mode . visual-line-mode) )    ; spell checking!

  :bind (:map global-map)
  :config
  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)
  )
(setq org-startup-folded 'fold)

(setq org-contacts-vcard-file "contacts.vcard")
(use-package org-contacts
  :ensure t)



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
        ("l" "Link" entry (file+headline "~/org/link.org" "inbox")
         "** %\n"
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
  :ensure t)

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
(setq org-agenda-start-with-log-mode t)
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

(use-package toc-org ; auto-table of contents
  :ensure t
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh"))

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

(use-package org-cliplink 
  :ensure t)

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

(use-package hledger-mode
:ensure t)

;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . hledger-mode))

;; Provide the path to you journal file.
;; The default location is too opinionated.
(setq hledger-jfile "~/org/finance/2024.ledger")
(global-set-key (kbd "C-c C-l e") 'hledger-jentry)
(global-set-key (kbd "C-c C-l l") 'hledger-run-command)

(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

(use-package cider
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(use-package sly :ensure t)

(use-package magit :ensure t
  :bind ((
          "C-x g" . magit-status
          ))
  )

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

(load-file (expand-file-name "modules/tools/term.el" user-emacs-directory))

(load-file (expand-file-name "modules/tools/work/work.el" user-emacs-directory))

(load-file (expand-file-name "modules/tools/gptel.el" user-emacs-directory))

(load-file (expand-file-name "modules/tools/rss.el" user-emacs-directory))

(setq org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
