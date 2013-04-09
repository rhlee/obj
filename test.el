(require 'ert)
(load-file "obj.el")
(ert-delete-all-tests)

(ert-deftest obj-01-new-object-is-hash-table ()
  (should
    (hash-table-p (obj))))

(ert t)
