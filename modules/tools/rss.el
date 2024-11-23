(use-package elfeed 
  :ensure t)
(global-set-key (kbd "C-x w") 'elfeed)

;; Somewhere in your .emacs file
(setq elfeed-feeds
      '(
        "https://planet.emacslife.com/atom.xml"
        "https://reddit.com/r/de.rss"
        "https://www.tagesschau.de/index~rss2.xml"
        "https://www.handelsblatt.com/contentexport/feed/schlagzeilen"
        "https://www.handelsblatt.com/contentexport/feed/politik"
        )
)
