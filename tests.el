;;; tests.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Correl Roush
;;
;; Author: Correl Roush <correlr@zephyrus>
;; Maintainer: Correl Roush <correlr@zephyrus>
;; Created: June 11, 2022
;; Modified: June 11, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/correlr/tests
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'ert)
(require 'org-roam-export)

(ert-deftest simple-test ()
  (should (= 1 1)))

(provide 'tests)
;;; tests.el ends here
