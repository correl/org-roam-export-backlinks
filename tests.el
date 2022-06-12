;;; tests.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Correl Roush
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'ert)
(require 'org-roam)
(require 'org-roam-export)
(require 'seq)

(setq org-roam-directory (expand-file-name "./test-slipbox")
      org-roam-db-location (expand-file-name "org-roam.db"))
(org-roam-update-org-id-locations)
(org-roam-db-sync)


(ert-deftest simple-test ()
  (should (= 1 1)))

(ert-deftest lorem-backlink-titles ()
  (should (equal '("Ipsum > II")
                 (seq-map #'org-roam-export-backlink-title (org-roam-backlinks-get (org-roam-node-from-id "d12a1ce4-3199-42f4-b39b-b68c03458669") :unique t)))))

(provide 'tests)
;;; tests.el ends here
