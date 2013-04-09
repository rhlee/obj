(require 'ert)

(defun obj-test-generate-random-string (limit)
  (let ((s ""))
    (dotimes (number (+ (random limit) 1) s)
      (setq s (concat s (char-to-string (+ 97 (random 26))))))))

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

(ert-deftest obj-test-05-set-returns-same-value ()
  (let
    ((property-symbol (intern (obj-test-generate-random-string 10)))
      (property-value (obj-test-generate-random-string 10))
      (object (obj)))
    (should (eq
      (obj object :set property-symbol property-value)
      property-value))))

(ert t)
