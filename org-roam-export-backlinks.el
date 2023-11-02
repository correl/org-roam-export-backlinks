;;; org-roam-export-backlinks.el --- Include backlinks in exported org documents -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Correl Roush
;;
;; Author: Correl Roush <correl@gmail.com>
;; Maintainer: Correl Roush <correl@gmail.com>
;; Created: June 10, 2022
;; Modified: October 29, 2022
;; Version: 0.0.2
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/correlr/org-roam-export
;; Package-Requires: ((emacs "25.1") (org-roam "2.2.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org-roam)

(defun org-roam-export-backlinks-title (backlink)
  "Get exportable title of BACKLINK."
  (mapconcat #'identity
             (append
              (list (org-roam-node-title (org-roam-backlink-source-node backlink)))
              (plist-get (org-roam-backlink-properties backlink) :outline))
             " > "))

(defun org-roam-export-backlinks-link (backlink)
  "Get Org link to BACKLINK."
  (concat "id:" (org-roam-node-id (org-roam-backlink-source-node backlink))))

(defun org-roam-export-backlinks--excerpt (&optional buffer element)
  "Extract an excerpt from ELEMENT in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((element (or element (org-element-at-point))))
      (if (eq 'headline (org-element-type element))
          (progn (org-mark-subtree)
                 (buffer-substring (point) (mark)))
        (if-let ((begin (org-element-property :contents-begin element))
                 (end (org-element-property :contents-end element)))
            (buffer-substring begin end)
          "")))))

(defun org-roam-export-backlinks-excerpt (backlink)
  "Get the Org element containing the link from BACKLINK as an excerpt."
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (org-roam-node-file (org-roam-backlink-source-node backlink)))
    (goto-char (org-roam-backlink-point backlink))
    (org-roam-export-backlinks--excerpt)))

(defun org-roam-export-backlinks--format (link title excerpt)
  "Format a backlink with TITLE and EXCERPT for inclusion in an Org document."
  (with-temp-buffer
    (org-mode)
    (insert "** " (org-make-link-string link title) "\n\n")
    (if (string-match-p "^\*+ " excerpt) (org-paste-subtree 3 excerpt)
      (insert excerpt))
    (buffer-string)))

(defun org-roam-export-backlinks-format (backlink)
  "Format a BACKLINK for inclusion in an Org document."
  (org-roam-export-backlinks--format
   (org-roam-export-backlinks-link backlink)
   (org-roam-export-backlinks-title backlink)
   (org-roam-export-backlinks-excerpt backlink)))

(defun org-roam-export-backlinks-preprocessor (backend)
  "Append org-roam backlinks with content when applicable before
passing to the org export BACKEND."
  (when-let ((node (org-roam-node-at-point)))
    (let ((backlinks (org-roam-backlinks-get node)))
      (when backlinks
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n"
                          (string-join (mapcar #'org-roam-export-backlinks-format backlinks)))))))))

(provide 'org-roam-export-backlinks)
;;; org-roam-export-backlinks.el ends here
