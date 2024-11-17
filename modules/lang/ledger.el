(use-package hledger-mode
:ensure t)

;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . hledger-mode))

;; Provide the path to you journal file.
;; The default location is too opinionated.
(setq hledger-jfile "~/org/finance/2024.ledger")
(global-set-key (kbd "C-c C-l e") 'hledger-jentry)
(global-set-key (kbd "C-c C-l l") 'hledger-run-command)
