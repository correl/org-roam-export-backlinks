;;; org-roam-export.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Correl Roush
;;
;; Author: Correl Roush <correlr@zephyrus>
;; Maintainer: Correl Roush <correlr@zephyrus>
;; Created: June 10, 2022
;; Modified: June 10, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/correlr/org-roam-export
;; Package-Requires: ((emacs "24.3") org-roam s seq)
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org-roam)
(require 's)
(require 'seq)

(defun org-roam-export-backlink-title (backlink)
  "Get exportable title of BACKLINK."
  (s-join " > "
          (seq-concatenate 'list
                           (list (org-roam-node-title (org-roam-backlink-source-node backlink)))
                           (plist-get (org-roam-backlink-properties backlink) :outline))))

(defun org-roam-export-backlink-link (backlink)
  "Get Org link to BACKLINK."
  (concat "id:" (org-roam-node-id (org-roam-backlink-source-node backlink))))

(provide 'org-roam-export)
;;; org-roam-export.el ends here
