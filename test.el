(require 'ert)
(load-file "obj.el")
(ert-delete-all-tests)

(ert-deftest obj-test-01-new-object-is-hash-table ()
  (should
    (hash-table-p (obj))))

(ert-deftest obj-test-02-first-argument-is-hash-table ()
  (should (equal
    (cdr (should-error
      (obj nil)
      :type 'obj-error))
    "first argument should ba a hash-table")))

(ert t)
