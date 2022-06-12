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
(org-roam-update-org-id-locations)
(org-roam-db-sync)


(ert-deftest simple-test ()
  (should (= 1 1)))

(ert-deftest lorem-backlink-titles ()
  (should (equal '("Ipsum > II")
                 (mapcar #'org-roam-export-backlink-title (org-roam-backlinks-get (org-roam-node-from-id "d12a1ce4-3199-42f4-b39b-b68c03458669") :unique t)))))

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
sodales enim eget tristique tempor.")
                 (mapcar #'org-roam-export-backlink-excerpt (org-roam-backlinks-get (org-roam-node-from-id "d12a1ce4-3199-42f4-b39b-b68c03458669") :unique t)))))

(provide 'tests)
;;; tests.el ends here
