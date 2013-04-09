(put 'obj-error 'error-conditions '(error obj-error))
(put 'obj-error 'error-message "obj error")

(defun obj (&rest args)
  (let
    ((return-value)
    (object)
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
                  (setq member (car args))
                  (setq return-value (car (cdr args))))
                ((symbolp command-or-member)
                  ())
                (t (signal 'obj-error
                  "second argument should be keyword symbol or symbol"))))
            (signal 'obj-error "command or member required after object"))
          return-value)
        (signal 'obj-error "first argument should ba a hash-table"))
      (make-hash-table))))
;;after command leave if
