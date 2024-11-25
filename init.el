;
;; (with-eval-after-load 'package
  ;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; If you want to turn off the welcome screen, uncomment this
(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

;(icomplete-vertical-mode)
;(fido-vertical-mode)
;(setopt icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
;;
;; (setopt indent-tabs-mode nil)
;; (setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun apply-modus-operandi-tinted-palette (frame)
  "Apply the catppuccin theme to the FRAME."
  (with-selected-frame frame
    (load-theme 'modus-operandi-tinted t)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'apply-modus-operandi-tinted-palette)
  (load-theme 'modus-operandi-tinted t))


(load-file (expand-file-name "modules/tools/eglot.el" user-emacs-directory))
(load-file (expand-file-name "modules/tools/devdocs.el" user-emacs-directory))
(load-file (expand-file-name "modules/tools/term.el" user-emacs-directory))
(load-file (expand-file-name "modules/tools/work/work.el" user-emacs-directory))
(load-file (expand-file-name "modules/tools/gptel.el" user-emacs-directory))
(load-file (expand-file-name "modules/tools/rss.el" user-emacs-directory))

;;; Load Languages
(load-file (expand-file-name "modules/lang/markdown.el" user-emacs-directory))
(load-file (expand-file-name "modules/lang/org.el" user-emacs-directory))
(load-file (expand-file-name "modules/lang/ledger.el" user-emacs-directory))
(load-file (expand-file-name "modules/lang/clojure.el" user-emacs-directory))

;;load defaults

(load-file (expand-file-name "modules/config/default.el" user-emacs-directory))

(fido-mode 1)
(fido-vertical-mode 1)
(ido-mode 1)
(ido-everywhere 1)

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
	      (html-mode . html-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
))

  
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (java "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
     (clojure "https://github.com/sogaiu/tree-sitter-clojure")
     ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment the (load-file …) lines or copy code from the extras/ elisp files
;; as desired

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
;(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;; Packages for software development
;(load-file (expand-file-name "extras/dev.el" user-emacs-directory))

;; Vim-bindings in Emacs (evil-mode configuration)
;(load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))

;; Org-mode configuration
;; WARNING: need to customize things inside the elisp file before use! See
;; the file extras/org-intro.txt for help.
;(load-file (expand-file-name "extras/org.el" user-emacs-directory))

;; Email configuration in Emacs
;; WARNING: needs the `mu' program installed; see the elisp file for more
;; details.
;(load-file (expand-file-name "extras/email.el" user-emacs-directory))

;; Tools for academic researchers
;(load-file (expand-file-name "extras/researcher.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    '("24b6ade0e3cabdfee9fa487961b089d059e048d77fe13137ea4788c1b62bd99d"
      default))
 '(hledger-currency-string "EUR")
 '(hledger-extra-args "")
 '(hledger-extrapolate-savings-period 12)
 '(hledger-extrapolate-savings-rate 7.0)
 '(hledger-ratios-assets-accounts "Assets")
 '(hledger-ratios-debt-accounts "Liabilities")
 '(hledger-ratios-essential-expense-accounts "")
 '(hledger-ratios-income-accounts
    "Revenue:Gehalt Revenue:Sonstiges Revenue:Netflix Revenue:Refunds")
 '(hledger-ratios-liquid-asset-accounts "Assets:Cash Assets:Checking Assets:Savings")
 '(hledger-show-expanded-report t)
 '(hledger-top-asset-account "Assets")
 '(hledger-top-expense-account "Expenses")
 '(hledger-top-income-account "Revenue")
 '(hledger-year-of-birth 1997)
 '(org-agenda-files
    '("/Users/taradruffel/org/albion/albion.org"
      "/Users/taradruffel/org/configs/config.org"
      "/Users/taradruffel/org/configs/contacts.org"
      "/Users/taradruffel/org/contacts/contacts.org"
      "/Users/taradruffel/org/notes/areas/clojure/clojure_faq.org"
      "/Users/taradruffel/org/notes/areas/cooking/recipes.org"
      "/Users/taradruffel/org/notes/areas/fitness/exercise.org"
      "/Users/taradruffel/org/notes/areas/work/meetings/estimations.org"
      "/Users/taradruffel/org/notes/areas/work/meetings/notes.org"
      "/Users/taradruffel/org/notes/areas/work/mysql.org"
      "/Users/taradruffel/org/notes/areas/work/notes.org"
      "/Users/taradruffel/org/notes/areas/work/other_links.org"
      "/Users/taradruffel/org/notes/areas/inventur.org"
      "/Users/taradruffel/org/notes/projects/game/README.org"
      "/Users/taradruffel/org/notes/projects/projects.org"
      "/Users/taradruffel/org/notes/resources/codesign.org"
      "/Users/taradruffel/org/notes/resources/morning-routine.org"
      "/Users/taradruffel/org/notes/resources/quality_brands.org"
      "/Users/taradruffel/org/notes/resources/reading_list.org"
      "/Users/taradruffel/org/notes/index.org"
      "/Users/taradruffel/org/calendar_work.org"
      "/Users/taradruffel/org/inbox.org"
      "/Users/taradruffel/org/inventur.org"
      "/Users/taradruffel/org/link.org"
      "/Users/taradruffel/org/recurring_calendar.org"
      "/Users/taradruffel/org/todos.org"))
 '(org-safe-remote-resources
    '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"
      "\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-bigblow\\.setup\\'"))
 '(org-timeblock-current-time-indicator t)
 '(org-timeblock-scale-options nil)
 '(package-selected-packages
    '(avy calfw calfw-org cape catppuccin-theme cider consult
          corfu-terminal devdocs eat elfeed emacs-everywhere
          exec-path-from-shell expand-region gptel hledger-mode
          kind-icon ledger-mode magit markdown-mode
          modus-operandi-tinted-palette orderless org-cliplink
          org-contacts org-modern org-super-agenda org-timeblock
          toc-org vertico wgrep)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

