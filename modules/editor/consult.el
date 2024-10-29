;; consult is used as a fuzzy finder and grepper/ use like telescope or fzf.nvim

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop ;; Not sure I need that
         ;; Searching
         ("M-s r" . find-grep)
	     ("M-s f" . consult-find) ;; find File
         ("M-s /" . consult-line)       ; Alternative: rebind C-s to use
         ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi) ; isearch to M-s s 
         ("M-s o" . consult-outline)

         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-istory)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

