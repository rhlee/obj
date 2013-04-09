(put 'obj-error 'error-conditions '(error obj-error))
(put 'obj-error 'error-message "obj error")

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
                    (signal 'obj-error "member required after command")))
                ((symbolp command-or-member)
                  ())
                (t (signal 'obj-error
                  "second argument should be keyword symbol or symbol"))))
            (signal 'obj-error "command or member required after object"))
          (cond
            ((eq command :set)
              (puthash :member (car args) object))
            ((eq command :get)
              (gethash :member object))))
        (signal 'obj-error "first argument should ba a hash-table"))
      (make-hash-table))))
