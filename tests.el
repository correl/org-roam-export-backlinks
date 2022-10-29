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
(require 'org-roam-export-backlinks)

(setq org-roam-directory (expand-file-name "./test-slipbox")
      org-roam-db-location (expand-file-name "./test-slipbox/org-roam.db"))
(org-roam-update-org-id-locations)
(org-roam-db-sync)


(ert-deftest lorem-backlink-titles ()
  (should (equal '("Ipsum > II")
                 (mapcar #'org-roam-export-backlinks-title (org-roam-backlinks-get (org-roam-node-from-id "d12a1ce4-3199-42f4-b39b-b68c03458669") :unique t)))))

(ert-deftest ipsum-backlink-titles ()
  (should (equal '("Lorem")
                 (mapcar #'org-roam-export-backlinks-title (org-roam-backlinks-get (org-roam-node-from-id "e6c17c1a-6b05-40d2-a01f-b147633c51b1") :unique t)))))

(ert-deftest lorem-backlink-link ()
  (should (equal '("id:e6c17c1a-6b05-40d2-a01f-b147633c51b1")
                 (mapcar #'org-roam-export-backlinks-link (org-roam-backlinks-get (org-roam-node-from-id "d12a1ce4-3199-42f4-b39b-b68c03458669") :unique t)))))

(ert-deftest ipsum-backlink-link ()
  (should (equal '("id:d12a1ce4-3199-42f4-b39b-b68c03458669")
                 (mapcar #'org-roam-export-backlinks-link (org-roam-backlinks-get (org-roam-node-from-id "e6c17c1a-6b05-40d2-a01f-b147633c51b1") :unique t)))))

(defmacro with-test-document (&rest body)
  `(with-temp-buffer
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
    ,@body))

(ert-deftest excerpt-opening-paragraph-test ()
  (with-test-document
   (goto-line 6)
   (should (equal "Opening paragraph.\n" (org-roam-export-backlinks--excerpt)))))

(ert-deftest excerpt-heading-paragraph-test ()
  (with-test-document
   (goto-line 10)
   (should (equal "  Heading one paragraph.\n" (org-roam-export-backlinks--excerpt)))))

(ert-deftest excerpt-list-test ()
  (with-test-document
    (goto-line 14)
    (should (equal "  - List item 1\n  - List item 2\n" (org-roam-export-backlinks--excerpt)))))

(ert-deftest excerpt-heading-with-subheadings-test ()
  (with-test-document
   (goto-line 8)
    (should (equal "* Heading One

  Heading one paragraph.

** Subheading One A

  - List item 1
  - List item 2

" (org-roam-export-backlinks--excerpt)))))

(ert-deftest excerpt-simple-heading-test ()
  (with-test-document
   (goto-line 17)
   (should (equal "* Heading Two\n\n  Heading two paragraph.\n" (org-roam-export-backlinks--excerpt)))))

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
                 (mapcar #'org-roam-export-backlinks-excerpt (org-roam-backlinks-get (org-roam-node-from-id "d12a1ce4-3199-42f4-b39b-b68c03458669") :unique t)))))

(ert-deftest ipsum-backlink-excerpt ()
  (should (equal '("Lorem [[id:e6c17c1a-6b05-40d2-a01f-b147633c51b1][ipsum]] dolor sit amet, consectetur adipiscing elit. Mauris eget viverra mi.
Duis eget dui id tellus fermentum vehicula. Duis tincidunt quam vel erat
bibendum commodo. Phasellus in justo vitae magna commodo rhoncus id quis justo.
Morbi id malesuada nisi. Praesent ipsum velit, commodo vel bibendum vitae,
dignissim in magna. Pellentesque vehicula enim ante, interdum laoreet dolor
venenatis eget. Proin laoreet nulla a enim bibendum finibus. Proin mattis
lobortis quam non eleifend. Pellentesque vitae imperdiet nisl.\n")
                 (mapcar #'org-roam-export-backlinks-excerpt (org-roam-backlinks-get (org-roam-node-from-id "e6c17c1a-6b05-40d2-a01f-b147633c51b1") :unique t)))))

(ert-deftest format-backlink-test ()
  (should (equal "** [[id:34cf17c6-f804-4adb-b386-3c6dfb83cfad][Heading > Subheading]]

Paragraph text.
"
                 (org-roam-export-backlinks--format
                  "id:34cf17c6-f804-4adb-b386-3c6dfb83cfad"
                  "Heading > Subheading"
                  "Paragraph text.\n"))))

(ert-deftest format-backlink-with-subheadings ()
  (should (equal "** [[id:9428184d-5030-4e5b-91f6-b865d5cc311b][Heading]]

*** Subheading One

Paragraph text.

**** Subheading Two

***** Subheading Two A

Paragraph text.
"
                 (org-roam-export-backlinks--format
                  "id:9428184d-5030-4e5b-91f6-b865d5cc311b"
                  "Heading"
                  "** Subheading One

Paragraph text.

*** Subheading Two

**** Subheading Two A

Paragraph text.
"))))

(provide 'tests)
;;; tests.el ends here
