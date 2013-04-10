(put 'obj-error 'error-conditions '(error obj-error))
(put 'obj-error 'error-message "obj error")

(defun signal-obj-error (data)
  (let* ((errors
    '((first-argument . "first argument should be a hash-table")
      (second-argument .
"second argument should be command (keyword symbol) or member (symbol)")
      (member-after-command . "member required after command")
      (invalid-command . "invalid command")
      (value-error .
"after member: :set takes 1 value exactly, :get takes no value")))
    (error-assoc (or (assoc data errors)
      '(unspecified-error . "unspecified error"))))
    (signal 'obj-error (list (car error-assoc) (cdr error-assoc)))))

(defun obj (&rest args)
  (let
    ((object)
    (command-or-member)
    (command)
    (member)
    (value-length))
    (if args
      (if (hash-table-p (setq object (car args)))
        (progn
          (setq args (cdr args))
          (if args
            (progn
              (setq command-or-member (car args))
              (setq args (cdr args))
              (cond
                ((keywordp command-or-member)
                  (setq command command-or-member)
                  (if args
                    (progn
                      (setq member (car args))
                      (setq args (cdr args)))
                    (signal-obj-error 'member-after-command)))
                ((symbolp command-or-member)
                  ())
                (t (signal-obj-error 'second-argument))))
            (signal-obj-error 'second-argument))
          (setq value-length (length args))
          (cond
            ((eq command :set)
              (if (eq value-length 1)
                (puthash :member (car args) object)
                (signal-obj-error 'value-error)))
            ((eq command :get)
              (if (eq value-length 0)
                (gethash :member object)
                (signal-obj-error 'value-error)))
            (t (signal-obj-error 'invalid-command))))
        (signal-obj-error 'first-argument))
      (make-hash-table))))
;;move args down (into cond)
;;after member
