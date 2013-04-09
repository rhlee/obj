(put 'obj-error 'error-conditions '(error obj-error))
(put 'obj-error 'error-message "obj error")

(defun obj (&rest args)
  (if args
    (signal 'obj-error "first argument should ba a hash-table")
    (make-hash-table)))
