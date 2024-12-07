;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-ql" "0.8.10"
  "Org Query Language, search command, and agenda-like view."
  '((emacs            "27.1")
    (compat           "29.1")
    (dash             "2.18.1")
    (f                "0.17.2")
    (map              "2.1")
    (org              "9.0")
    (org-super-agenda "1.2")
    (ov               "1.0.6")
    (peg              "1.0.1")
    (s                "1.12.0")
    (transient        "0.1")
    (ts               "0.2-pre"))
  :url "https://github.com/alphapapa/org-ql"
  :commit "9c53c1bddfcbda3475ffd8810f012be6de07d963"
  :revdesc "v0.8.10-0-g9c53c1bddfcb"
  :keywords '("hypermedia" "outlines" "org" "agenda")
  :authors '(("Adam Porter" . "adam@alphapapa.net"))
  :maintainers '(("Adam Porter" . "adam@alphapapa.net")))
