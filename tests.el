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
      org-roam-db-location (expand-file-name "./test-slipbox/org-roam.db"))
(org-roam-update-org-id-locations)
(org-roam-db-sync)


(ert-deftest lorem-backlink-titles ()
  (should (equal '("Ipsum > II")
                 (mapcar #'org-roam-export-backlink-title (org-roam-backlinks-get (org-roam-node-from-id "d12a1ce4-3199-42f4-b39b-b68c03458669") :unique t)))))

(ert-deftest ipsum-backlink-titles ()
  (should (equal '("Lorem")
                 (mapcar #'org-roam-export-backlink-title (org-roam-backlinks-get (org-roam-node-from-id "e6c17c1a-6b05-40d2-a01f-b147633c51b1") :unique t)))))

(ert-deftest lorem-backlink-link ()
  (should (equal '("id:e6c17c1a-6b05-40d2-a01f-b147633c51b1")
                 (mapcar #'org-roam-export-backlink-link (org-roam-backlinks-get (org-roam-node-from-id "d12a1ce4-3199-42f4-b39b-b68c03458669") :unique t)))))

(ert-deftest ipsum-backlink-link ()
  (should (equal '("id:d12a1ce4-3199-42f4-b39b-b68c03458669")
                 (mapcar #'org-roam-export-backlink-link (org-roam-backlinks-get (org-roam-node-from-id "e6c17c1a-6b05-40d2-a01f-b147633c51b1") :unique t)))))

(ert-deftest excerpt-tests ()

  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:
:ID:       e44c8b00-f90d-4fcc-b8a1-742b659ff252
:END
#+TITLE: Test Document

Opening paragraph.

* Heading One

  Heading one paragraph.

** Subheading One A

  - List item 1
  - List item 2

* Heading Two

  Heading two paragraph.
")
    ;; Elements
    (goto-line 6)
    (should (equal "Opening paragraph.\n" (org-roam-export--excerpt)))
    (goto-line 10)
    (should (equal "  Heading one paragraph.\n" (org-roam-export--excerpt)))
    (goto-line 14)
    (should (equal "  - List item 1\n  - List item 2\n" (org-roam-export--excerpt)))
    ;; Headings
    (goto-line 8)
    (should (equal "  Heading one paragraph.

** Subheading One A

  - List item 1
  - List item 2
" (org-roam-export--excerpt)))
    (goto-line 17)
    (should (equal "  Heading two paragraph.\n" (org-roam-export--excerpt)))))

(ert-deftest lorem-backlink-excerpt ()
  (should (equal '("Aliquam [[id:d12a1ce4-3199-42f4-b39b-b68c03458669][lorem]] ante, suscipit a lorem molestie, aliquet elementum eros. Proin
mattis lacus nec dapibus auctor. Donec lacinia finibus ex vitae tempor.
Suspendisse blandit, justo vitae placerat lacinia, eros tortor convallis nisi,
fermentum sodales lorem augue at ligula. Nulla facilisi. Curabitur vel convallis
tellus, a luctus mi. Donec sollicitudin erat erat, vel condimentum mauris tempus
eget. Pellentesque hendrerit suscipit risus eu fermentum. Vivamus non urna
commodo, lacinia odio vitae, blandit metus. Nam et tempus ipsum. Aenean lobortis
mauris sit amet lorem accumsan blandit. Fusce eleifend, tellus non tristique
auctor, ligula justo varius dolor, id bibendum nulla elit ac dui. Vestibulum
sodales enim eget tristique tempor.\n")
                 (mapcar #'org-roam-export-backlink-excerpt (org-roam-backlinks-get (org-roam-node-from-id "d12a1ce4-3199-42f4-b39b-b68c03458669") :unique t)))))

(ert-deftest ipsum-backlink-excerpt ()
  (should (equal '("Lorem [[id:e6c17c1a-6b05-40d2-a01f-b147633c51b1][ipsum]] dolor sit amet, consectetur adipiscing elit. Mauris eget viverra mi.
Duis eget dui id tellus fermentum vehicula. Duis tincidunt quam vel erat
bibendum commodo. Phasellus in justo vitae magna commodo rhoncus id quis justo.
Morbi id malesuada nisi. Praesent ipsum velit, commodo vel bibendum vitae,
dignissim in magna. Pellentesque vehicula enim ante, interdum laoreet dolor
venenatis eget. Proin laoreet nulla a enim bibendum finibus. Proin mattis
lobortis quam non eleifend. Pellentesque vitae imperdiet nisl.\n")
                 (mapcar #'org-roam-export-backlink-excerpt (org-roam-backlinks-get (org-roam-node-from-id "e6c17c1a-6b05-40d2-a01f-b147633c51b1") :unique t)))))

(provide 'tests)
;;; tests.el ends here
