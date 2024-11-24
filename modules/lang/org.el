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
(use-package org-timeblock :ensure t)
(use-package calfw :ensure t)
(use-package calfw-org :ensure t)
;; Agenda variables
(setq org-directory "~/org/") ; Non-absolute paths for agenda and

(setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))

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

(use-package org-modern
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
(add-hook 'org-mode-hook #'org-modern-mode)

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

