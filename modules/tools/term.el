(use-package eshell
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
