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

(setq org-roam-directory (expand-file-name "./test-slipbox")
      org-roam-db-location (expand-file-name "org-roam.db"))
(org-roam-db-sync)


(ert-deftest simple-test ()
  (should (= 1 1)))

(provide 'tests)
;;; tests.el ends here
