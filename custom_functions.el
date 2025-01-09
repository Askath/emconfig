
(defun generate-tags-for-project ()
  "Generate tags file for the current project root."
  (interactive)
  (let ((project-root (project-root (project-current t))))
    (if project-root
        (let ((default-directory project-root))
          (shell-command "ctags -e -R --languages=JavaScript,TypeScript *")
          (message "Tags file generated in project root: %s" project-root))
      (message "Not in a project!"))))

(defun generate-tags-for-cwd ()
  "Generate tags file for all files starting from the current working directory."
  (interactive)
  (let ((default-directory (or default-directory (getenv "PWD"))))
    (if default-directory
        (progn
          (shell-command "find . -type f -print | etags -")
          (message "ctags -e -R *" default-directory))
      (message "Cannot determine the current working directory!"))))

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; Calendar showing org-agenda entries
(defun my-open-calendar-agenda ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "medium purple"))
   :view 'block-day))

(defun my-open-calendar-week ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "medium purple"))
   :view 'block-week))


(defun blocks-day-view ()
  (interactive)
    (cfw:cp-set-view (cfw:cp-get-component) 'block-day)
)

(defun blocks-week-view ()
  (interactive)
    (cfw:cp-set-view (cfw:cp-get-component) 'block-week)
)

(with-eval-after-load 'calfw
  (define-key cfw:calendar-mode-map (kbd "D") 'blocks-day-view)
  (define-key cfw:calendar-mode-map (kbd "W") 'blocks-week-view)
)


(defun treesit-install-all () 
  interactive
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  )

(global-set-key (kbd "C-c ot") 'eat)

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
    (let ((output-buffer (get-buffer-create "*VPN Status*")))
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
