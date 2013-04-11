(put 'obj-error 'error-conditions '(error obj-error))
(put 'obj-error 'error-message "obj error")

(setq obj-nil (make-symbol "obj-nil"))

(defun signal-obj-error (data command)
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
    (signal 'obj-error (list (car error-assoc) (cdr error-assoc)
      (cons 'command command)))))

(defun obj (&rest args)
  (let
    ((object)
    (command-or-member)
    (command)
    (member)
    (value-length)
    (fetched-value))
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
                    (signal-obj-error 'member-after-command command)))
                ((symbolp command-or-member)
                  (setq fetched-value
                    (gethash command-or-member object obj-nil))
                  (if (eq fetched-value obj-nil)
                    (progn
                      (setq command :set)
                      (setq member command-or-member))
                    (setq command :get)))
                (t (signal-obj-error 'second-argument command))))
            (signal-obj-error 'second-argument command ))
          (setq value-length (length args))
          (cond
            ((eq command :set)
              (if (eq value-length 1)
                (puthash member (car args) object)
                (signal-obj-error 'value-error command)))
            ((eq command :get)
              (if (eq value-length 0)
                (or fetched-value (gethash member object))
                (signal-obj-error 'value-error command)))
            (t (signal-obj-error 'invalid-command command))))
        (signal-obj-error 'first-argument command))
      (make-hash-table))))
;;move args down (into cond)
