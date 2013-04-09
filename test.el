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

(ert-deftest obj-test-03-command-or-member-required-after-object ()
  (should (equal
    (cdr (should-error
      (obj (obj))
      :type 'obj-error))
    "command or member required after object")))

(ert-deftest obj-test-04-2nd-argument-is-symbol ()
  (should (equal
    (cdr (should-error
      (obj (obj) 1)
      :type 'obj-error))
    "second argument should be keyword symbol or symbol"))
  (obj (obj) :somecommand)
  (obj (obj) 'somemember))

(ert t)
