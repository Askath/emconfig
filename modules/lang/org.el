;;; Emacs Bedrock;
;;; Extra config: Org-mode starter config

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; Org-mode is a fantastically powerful package. It does a lot of things, which
;;; makes it a little difficult to understand at first.
;;;
;;; We will configure Org-mode in phases. Work with each phase as you are
;;; comfortable.
;;;
;;; YOU NEED TO CONFIGURE SOME VARIABLES! The most important variable is the
;;; `org-directory', which tells org-mode where to look to find your agenda
;;; files.

;;; See "org-intro.txt" for a high-level overview.

;;; Contents:
;;;
;;;  - Critical variables
;;;  - Phase 1: editing and exporting files
;;;  - Phase 2: todos, agenda generation, and task tracking
;;;  - Phase 3: extensions (org-roam, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables need to be set for Org-mode's full power to be unlocked!
;;;
;;; You can read the documentation for any variable with `C-h v'. If you have
;;; Consult configured (see the `base.el' file) then it should help you find
;;; what you're looking for.

;;; Phase 1 variables

;;; Phase 2 variables

;; Agenda variables
(setq org-directory "~/org/") ; Non-absolute paths for agenda and

(setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))

;; ;; Default tags
;; (setq org-tag-alist '(
;;                       ;; locale
;;                       (:startgroup)
;;                       ("home" . ?h)
;;                       ("work" . ?w)
;;                       ("school" . ?s)
;;                       (:endgroup)
;;                       (:newline)
;;                       ;; scale
;;                       (:startgroup)
;;                       ("one-shot" . ?o)
;;                       ("project" . ?j)
;;                       ("tiny" . ?t)
;;                       (:endgroup)
;;                       ;; misc
;;                       ("meta")
;;                       ("review")
;;                       ("reading")))

;; ;; Org-refile: where should org-refile look?
;; (setq org-refile-targets 'FIXME)

;; ;;; Phase 3 variables

;; ;; Org-roam variables
;; (setq org-roam-directory "~/Documents/org-roam/")
;; (setq org-roam-index-file "~/Documents/org-roam/index.org")

;; ;;; Optional variables

;; ;; Advanced: Custom link types
;; ;; This example is for linking a person's 7-character ID to their page on the
;; ;; free genealogy website Family Search.
;; (setq org-link-abbrev-alist
;;       '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;   Phase 1: editing and exporting files
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode))    ; spell checking!

  :bind (:map global-map)
                :config
  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;   Phase 2: todos, agenda generation, and task tracking
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Yes, you can have multiple use-package declarations. It's best if their
;; ;; configs don't overlap. Once you've reached Phase 2, I'd recommend merging the
;; ;; config from Phase 1. I've broken it up here for the sake of clarity.
;; (use-package org
;;   :config
;;   ;; Instead of just two states (TODO, DONE) we set up a few different states
;;   ;; that a task can be in. Run
;;   ;;     M-x describe-variable RET org-todo-keywords RET
;;   ;; for documentation on how these keywords work.
;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))

;;   ;; Refile configuration
;;   (setq org-outline-path-complete-in-steps nil)
;;   (setq org-refile-use-outline-path 'file)

;;   (setq org-capture-templates
;;         '(("c" "Default Capture" entry (file "inbox.org")
;;            "* TODO %?\n%U\n%i")
;;           ;; Capture and keep an org-link to the thing we're currently working with
;;           ("r" "Capture with Reference" entry (file "inbox.org")
;;            "* TODO %?\n%U\n%i\n%a")
;;           ;; Define a section
;;           ("w" "Work")
;;           ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
;;            "** TODO %?\n%U\n%i\n%a")
;;           ("wr" "Work report" entry (file+headline "work.org" "Reports")
;;            "** TODO %?\n%U\n%i\n%a")))

;;   ;; An agenda view lets you see your TODO items filtered and
;;   ;; formatted in different ways. You can have multiple agenda views;
;;   ;; please see the org-mode documentation for more information.
;;   (setq org-agenda-custom-commands
;;         '(("n" "Agenda and All Todos"
;;            ((agenda)
;;             (todo)))
;;           ("w" "Work" agenda ""
;;            ((org-agenda-files '("work.org")))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;   Phase 3: extensions
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; TODO(setq! org-contacts-files (list "~/contacts/contacts.org"))
(setq org-contacts-vcard-file "contacts.vcard")


(use-package org-contacts
  :ensure t)

(global-set-key (kbd "C-c ns") 'org-search-view)
(global-set-key (kbd "C-c na") 'org-agenda)
(global-set-key (kbd "C-c x") 'org-capture)
(global-set-key (kbd "C-c nf") 'ido-find-file-in-dir)
(global-set-key (kbd "C-c n f") (lambda () (interactive) (consult-find "~/org")))

(setq org-capture-templates
      '(("a" "Appointment" entry (file+headline "~/org/calendar.org" "schedules")
         "* TODO %?\n  %i\n")
        ("t" "Todo Private" entry (file+headline "~/org/calendar.org" "Private")
         "** TODO %?\n  %i\n")
        ("T" "Todo Work" entry (file+headline "~/org/calendar.org" "Work")
         "** TODO %?\n  %i\n")
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

(setq org-contacts-files (list "~/contacts/contacts.org"))
(setq org-contacts-vcard-file "contacts.vcard")

(use-package org-super-agenda
  :ensure t)

(use-package org-modern
  :ensure t)

(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines nil
        org-agenda-include-scheduled nil
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil ;; i.e. today
        org-agenda-span 1
        org-agenda-start-on-weekday nil)
  (setq org-agenda-custom-commands
        '(
          ("c" "Today"
           ((agenda "" (
                        (org-agenda-span 1)
                        (org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '(
                           (:name "Today"
                            :time-grid t
                            :scheduled today
                            :order 1)
                           (:name "Habit"
                            :habit t
                            :tag "habit"
                            :order 2)
                           (:name "Recurring"
                            :time-grid t
                            :tag "recurring"
                            :order 4)
                           (:discard (:scheduled future :deadline t))
                           ))))
            ;; (alltodo "" (
            ;;              (org-agenda-overriding-header "")
            ;;              (org-super-agenda-groups
            ;;               '(
            ;;                 (:name "Privat"
            ;;                  :tag "private"
            ;;                  :order 3)
            ;;                 (:name "Work"
            ;;                  :tag "work"
            ;;                  :order 4)
            ;;                 (:name "Other"
            ;;                  :order 5)
            ;;                 (:discard (:tag "appointment" :tag "habit"))
            ;;                 ))))
            ))

          ("w" "Week"
           ((agenda "" (
                        (org-agenda-span 7)
                        (org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '(
                           (:name "--------- Workday -------"
                            :tag "workday"
                            :order 1)
                           (:name "Todo on this day"
                            :time-grid t
                            :order 2)
                           (:name "Recurring"
                            :tag "recurring"
                            :order 3)
                           (:name "Due This day"
                            :date t
                            :order 4)
                           ))))
            ))

          ("m" "Month"
           ((agenda "" (
                        (org-agenda-span 30)
                        (org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '(
                           (:name "--------- Workday -------"
                            :tag "workday"
                            :order 1)
                           (:name "Todo on this day"
                            :time-grid t
                            :order 2)
                           (:name "Recurring"
                            :tag "recurring"
                            :order 3)
                           (:name "Due This day"
                            :date t
                            :order 4)
                           ))))
            ))
          )))
:config
(org-super-agenda-mode)
(add-hook 'org-mode-hook #'org-modern-mode)

(use-package org-make-toc
:ensure t)

(require 'org-make-toc)

;; Default t
(setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
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
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))
