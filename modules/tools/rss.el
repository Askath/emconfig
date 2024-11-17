(use-package elfeed 
  :ensure t)
(global-set-key (kbd "C-x w") 'elfeed)

;; Somewhere in your .emacs file
(setq elfeed-feeds
      '(
        "https://planet.emacslife.com/atom.xml"
        "https://reddit.com/r/de.rss"
        )
)
