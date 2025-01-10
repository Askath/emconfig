;; Completion UI Framework
;; Enable vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package embark :ensure t)
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :config
  (setq corfu-auto t)
  (global-corfu-mode))

(use-package dirvish
  :ensure t
  :init 
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes
      '(vc-state subtree-state collapse git-msg file-time file-size))
  (require 'dirvish-peek)
  (dirvish-peek-mode)

  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;;;;


;; Finance
(use-package hledger-mode :ensure nil
  :demand t
  :config
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
  )

(use-package delsel
  :hook (after-init . delete-selection-mode) :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package spacious-padding :ensure t
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))
(spacious-padding-mode)
  )

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
  :ensure t
  :config
  (which-key-mode))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind (("C-=" . er/expand-region)))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . visual-line-mode))

;; Calendar and org-mode extensions
(use-package calfw :ensure t)
(use-package calfw-org :ensure t)
(use-package calfw-blocks :ensure nil 
  :config 
  (setq calfw-blocks-default-event-length 0.25))

(use-package org
  :hook ((org-mode . visual-line-mode))
  :bind (:map global-map)
  :config
  (setq org-directory "~/org/") ; Non-absolute paths for agenda and
  (require 'oc-csl) ; citation support
  (add-to-list 'org-export-backends 'md)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-export-with-smart-quotes t)

  (setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))

  (setq org-startup-folded 'fold)

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
          )
        )

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
  )

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


(use-package aider
  :ensure nil 
  :config
  (setq aider-args '("--model" "gpt-4o-mini"))
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))



(use-package eshell
  :ensure t
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  (eat-term-name "iterm")
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

(use-package gptel 
  :ensure t
:config 
(setq gptel-default-mode 'text-mode))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)

  (read-extended-command-predicate #'command-completion-default-include-p))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))



(use-package paredit :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit"
    t)
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)
  (add-hook 'geiser-mode-hook           'enable-paredit-mode)
  )

(use-package org-super-agenda
  :ensure t
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
                           (:name "Evening Routine"
                                  :tag "evening_routine"
                                  :order 6)
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


(use-package helpful 
:ensure t
:config
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function))
