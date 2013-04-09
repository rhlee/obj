(put 'obj-error 'error-conditions '(error obj-error))
(put 'obj-error 'error-message "obj error")

(defun obj (&rest args)
  (let
    ((command-or-member))
    (if args
      (if (hash-table-p (car args))
        (progn
          (setq args (cdr args))
          (if args
            (progn
              (setq command-or-member (car args))
              (cond
                ((keywordp command-or-member)
                  ())
                ((symbolp command-or-member)
                  ())
                (t (signal 'obj-error
                  "second argument should be keyword symbol or symbol"))))
            (signal 'obj-error "command or member required after object")))
        (signal 'obj-error "first argument should ba a hash-table"))
      (make-hash-table))))
