;; Org-mode configurations
(use-package org
  :hook ((org-mode . visual-line-mode))
  :bind (:map global-map)
  :config
  (require 'oc-csl)
  (add-to-list 'org-export-backends 'md)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-export-with-smart-quotes t))

(setq org-directory "~/org/")
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
         "** TODO  %?\n ")
        ("l" "Link" entry (file+headline "~/org/link.org" "to watch/read")
         "** \n")
        ("n" "Note" entry (file "~/org/inbox.org")
         "* %?")
        ("m" "Meeting" entry (file "~/org/notes/areas/work/meetings/notes.org")
         "* %? ")))

(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil
        org-agenda-span 7
        org-agenda-start-on-weekday nil)
  (setq org-deadline-warning-days 90)
  (setq org-agenda-warning-days 90)
  (setq org-agenda-deadline-leaders '("Due: " "Due in %d days: " "Overdue %d days ago: "))
  (setq org-agenda-scheduled-leaders '("Scheduled: " "Scheduled in %d days: " "Scheduled %d days ago: "))
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-custom-commands
        '(("c" "Today"
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
                            ))))
           ))))
(org-super-agenda-mode)

(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "NEXT(n)"
         "PROJ(p)"
         "RESC(r)"
         "PROGR(s)"
         "WAIT(w)"
         "HOLD(h)"
         "IDEA(i)"
         "|"
         "DONE(d)"
         "KILL(k)")
        (sequence
         "[ ](T)"
         "[-](S)"
         "[?](W)"
         "|"
         "[X](D)")))

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
