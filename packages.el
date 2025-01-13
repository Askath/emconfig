

;; Completion UI Framework
;; Enable vertico
(require 'vertico)
(vertico-mode)

;; Optionally use the `orderless' completion style.
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(require 'embark)
;; Enable rich annotations using the Marginalia package
(require 'marginalia)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
(marginalia-mode)

(require 'corfu)
(setq corfu-auto t)
(global-corfu-mode)


(use-package dirvish
  :demand
  :config
  (require 'dirvish-widgets)
  (require 'dirvish-extras)
  (require 'dirvish-collapse)
  (require 'dirvish-emerge)
  (require 'dirvish-fd)
  (require 'dirvish-history)
  (require 'dirvish-icons)
  (require 'dirvish-ls)
  (require 'dirvish-narrow)
  (require 'dirvish-peek)
  ;; (require 'dirvish-quick-access-entries)
  (require 'dirvish-side)
  (require 'dirvish-subtree)
  (require 'dirvish-vc)
  (require 'dirvish-yank)
  (dirvish-override-dired-mode)
  (setq dirvish-attributes
	'(vc-state subtree-state collapse git-msg file-time file-size))
  (dirvish-peek-mode)
  (define-key global-map (kbd "C-c f") 'dirvish-fd)
  (define-key dirvish-mode-map (kbd "a") 'dirvish-quick-access)
  (define-key dirvish-mode-map (kbd "f") 'dirvish-file-info-menu)
  (define-key dirvish-mode-map (kbd "y") 'dirvish-yank-menu)
  (define-key dirvish-mode-map (kbd "N") 'dirvish-narrow)
  (define-key dirvish-mode-map (kbd "^") 'dirvish-history-last)
  (define-key dirvish-mode-map (kbd "h") 'dirvish-history-jump)
  (define-key dirvish-mode-map (kbd "s") 'dirvish-quicksort)
  (define-key dirvish-mode-map (kbd "v") 'dirvish-vc-menu)
  (define-key dirvish-mode-map (kbd "TAB") 'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "M-f") 'dirvish-history-go-forward)
  (define-key dirvish-mode-map (kbd "M-b") 'dirvish-history-go-backward)
  (define-key dirvish-mode-map (kbd "M-l") 'dirvish-ls-switches-menu)
  (define-key dirvish-mode-map (kbd "M-m") 'dirvish-mark-menu)
  (define-key dirvish-mode-map (kbd "M-t") 'dirvish-layout-toggle)
  (define-key dirvish-mode-map (kbd "M-s") 'dirvish-setup-menu)
  (define-key dirvish-mode-map (kbd "M-e") 'dirvish-emerge-menu)
  (define-key dirvish-mode-map (kbd "M-j") 'dirvish-fd-jump)
  )

;;;;


;; Finance
(require 'hledger-mode)
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . hledger-mode))
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

(use-package delsel
  :hook (after-init . delete-selection-mode) :ensure nil)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'spacious-padding)
(setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))
(spacious-padding-mode)

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

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; Calendar and org-mode extensions
(use-package calfw :ensure nil)
(use-package calfw-org :ensure nil)
(require 'calfw-blocks)
(setq calfw-blocks-default-event-length 0.25)

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

(use-package org-super-agenda :ensure nil)
(use-package toc-org
  :ensure nil
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh"))
(use-package org-cliplink :ensure nil)
(use-package org-upcoming-modeline
  :ensure nil
  :config
  (org-upcoming-modeline-mode))


(use-package aider
  :ensure nil 
  :config
  (setq aider-args '("--model" "gpt-4o-mini"))
  )


(load "~/.emacs.d/site-lisp/magit/magit-autoloads")


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
  :ensure nil
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



(use-package paredit :ensure nil
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
  :ensure nil
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
  :ensure nil
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))
