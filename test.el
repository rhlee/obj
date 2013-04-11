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
    ((key (intern (obj-test-generate-random-string 10)))
      (value (obj-test-generate-random-string 10))
      (object (obj)))
    (should (eq
      (obj object :set key value)
      value))))

(ert-deftest obj-test-06-get-returns-set-value ()
  (let
    ((key (intern (obj-test-generate-random-string 10)))
      (value (obj-test-generate-random-string 10))
      (object (obj)))
    (obj object :set key value)
    (should (eq
      (obj object :get key)
      value))))

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

(ert-deftest obj-test-12-mode-in-error-data ()
  (should (eq
    (cdr (assoc 'command
      (cdr (should-error
        (obj (obj) :get 'test 123 123)
        :type 'obj-error))))
    :get))
  (should (eq
    (cdr (assoc 'command
      (cdr (should-error
        (obj (obj) :set 'test 123 123)
        :type 'obj-error))))
    :set)))

(ert-deftest obj-test-13-auto-sets-pair ()
  (let
    ((key (intern (obj-test-generate-random-string 10)))
      (value (obj-test-generate-random-string 10))
      (object (obj)))
    (obj object key value)
    (should (eq
      (obj object :get key)
      value))))

(ert-deftest obj-test-14-auto-gets-pair ()
  (let
    ((key (intern (obj-test-generate-random-string 10)))
      (value (obj-test-generate-random-string 10))
      (object (obj)))
    (obj object key value)
    (should (eq
      (obj object :get key)
      value))))

(ert t)
;;skip command if nil in error
