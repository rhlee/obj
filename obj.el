(put 'obj-error 'error-conditions '(error obj-error))
(put 'obj-error 'error-message "obj error")

(defun signal-obj-error (data)
  (let* ((errors
    '((first-argument . "first argument should be a hash-table")
      (second-argument .
"second argument should be command (keyword symbol) or member (symbol)")
      (member-after-command . "member required after command")
      (invalid-command . "invalid command")
      (requires-value . ":set requires value")))
    (error-assoc (or (assoc data errors)
      '(unspecified-error . "unspecified error"))))
    (signal 'obj-error (list (car error-assoc) (cdr error-assoc)))))

(defun obj (&rest args)
  (let
    ((object)
    (command-or-member)
    (command)
    (member))
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
          (cond
            ((eq command :set)
              (if args
                (puthash :member (car args) object)
                (signal-obj-error 'requires-value)))
            ((eq command :get)
              (gethash :member object))
            (t (signal-obj-error 'invalid-command))))
        (signal-obj-error 'first-argument))
      (make-hash-table))))
;;move args down
