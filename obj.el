(put 'obj-error 'error-conditions '(error obj-error))
(put 'obj-error 'error-message "obj error")

(setq obj-nil (make-symbol "obj-nil"))
(defun error-if-obj-nil (value command)
  (if (eq value obj-nil)
    (signal-obj-error 'no-value command)
    value))

(defun signal-obj-error (data command)
  (let* ((errors
    '((first-argument . "first argument should be a hash-table")
      (second-argument .
"second argument should be command (keyword symbol) or member (symbol)")
      (member-after-command . "member required after command")
      (invalid-command . "invalid command")
      (value-error .
"after member: :set takes 1 value exactly, :get takes no value")
      (no-value . "key has no associated value")))
    (error-assoc (or (assoc data errors)
      '(unspecified-error . "unspecified error"))))
    (signal 'obj-error
      (cons
        (car error-assoc)
        (cons
          (cdr error-assoc)
          (if command
            (cons
              (cons
                'command
                command)
              nil)
            nil))))))

(defun obj (&rest args)
  (let
    ((object)
    (command-or-member)
    (command)
    (member)
    (value-length)
    (prefetched)
    (prefetched-value))
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
                  (setq prefetched-value
                    (gethash command-or-member object obj-nil))
                  (setq prefetched t)
                  (if (eq prefetched-value obj-nil)
                    (progn
                      (setq command :set)
                      (setq member command-or-member))
                    (setq command (if (functionp prefetched-value) :call :get))))
                (t (signal-obj-error 'second-argument command))))
            (signal-obj-error 'second-argument command ))
          (setq value-length (length args))
          (cond
            ((eq command :set)
              (if (eq value-length 1)
                (puthash member (car args) object)
                (signal-obj-error 'value-error command)))
            ((eq command :get)
              (if args
                (signal-obj-error 'value-error command)
                (if prefetched
                  prefetched-value
                  (error-if-obj-nil (gethash member object obj-nil) command))))
            ((eq command :call)
              (apply
                (if prefetched
                  prefetched-value
                  (error-if-obj-nil (gethash member object obj-nil) command))
                object args))
            ((eq command :with)
              (puthash member
                (apply (car args)
                  (error-if-obj-nil (gethash member object obj-nil) command)
                  (cdr args))
                object))
            ((eq command :rem)
              (if args
                (signal-obj-error 'value-error command)
                (remhash member object)))
            ((eq command :exist)
              (if args
                (signal-obj-error 'value-error command)
                (not (eq (gethash member object obj-nil) obj-nil))))
            ((eq command :get-or)
              (gethash member object (car args)))
            (t (signal-obj-error 'invalid-command command))))
        (signal-obj-error 'first-argument command))
      (make-hash-table))))
;;move args down (into cond)
;;args nil?
