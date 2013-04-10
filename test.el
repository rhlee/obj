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
  (should (eq
    (car (cdr (should-error
      (obj nil)
      :type 'obj-error)))
    'first-argument)))

(ert-deftest obj-test-03-command-or-member-required-after-object ()
  (should (eq
    (car (cdr (should-error
      (obj (obj))
      :type 'obj-error)))
    'second-argument)))

(ert-deftest obj-test-04-2nd-argument-is-symbol ()
  (should (eq
    (car (cdr (should-error
      (obj (obj) 1)
      :type 'obj-error)))
    'second-argument))
  (obj (obj) :get nil))

(ert-deftest obj-test-05-set-returns-same-value ()
  (let
    ((property-symbol (intern (obj-test-generate-random-string 10)))
      (property-value (obj-test-generate-random-string 10))
      (object (obj)))
    (should (eq
      (obj object :set property-symbol property-value)
      property-value))))

(ert-deftest obj-test-06-get-returns-set-value ()
  (let
    ((property-symbol (intern (obj-test-generate-random-string 10)))
      (property-value (obj-test-generate-random-string 10))
      (object (obj)))
    (obj object :set property-symbol property-value)
    (should (eq
      (obj object :get property-symbol)
      property-value))))

(ert-deftest obj-test-07-member-required-after-command ()
  (should (eq
    (car (cdr (should-error
      (obj (obj) :set)
      :type 'obj-error)))
    'member-after-command)))

(ert-deftest obj-test-08-invalid-command ()
  (should (eq
    (car (cdr (should-error
      (obj (obj) :test 'test)
      :type 'obj-error)))
    'invalid-command)))

(ert-deftest obj-test-09-set-requires-value ()
  (should (eq
    (car (cdr (should-error
      (obj (obj) :set 'test)
      :type 'obj-error)))
    'value-error)))

(ert-deftest obj-test-10-set-requires-1-value-exactly ()
  (should (eq
    (car (cdr (should-error
      (obj (obj) :set 'test 123 123)
      :type 'obj-error)))
    'value-error)))

(ert-deftest obj-test-11-get-takes-no-value ()
  (should (eq
    (car (cdr (should-error
      (obj (obj) :get 'test 123 123)
      :type 'obj-error)))
    'value-error)))

(ert t)
;;auto
;;show mode in error
