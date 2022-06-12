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
;; Package-Requires: ((emacs "24.4") (org-roam "2.2.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org-roam)

(defun org-roam-export-backlink-title (backlink)
  "Get exportable title of BACKLINK."
  (mapconcat #'identity
             (append
              (list (org-roam-node-title (org-roam-backlink-source-node backlink)))
              (plist-get (org-roam-backlink-properties backlink) :outline))
             " > "))

(defun org-roam-export-backlink-link (backlink)
  "Get Org link to BACKLINK."
  (concat "id:" (org-roam-node-id (org-roam-backlink-source-node backlink))))

(defun org-roam-export-backlink-excerpt (backlink)
  "Get the Org element containing the link from BACKLINK as an excerpt."
  (with-temp-buffer
    (insert-file-contents-literally (org-roam-node-file (org-roam-backlink-source-node backlink)))
    (goto-char (org-roam-backlink-point backlink))
    (let ((element (org-element-at-point)))
      (string-trim (buffer-substring (org-element-property :contents-begin element)
                                     (org-element-property :contents-end element))))))

(provide 'org-roam-export)
;;; org-roam-export.el ends here
