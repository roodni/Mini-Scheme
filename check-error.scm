(load "./mini.scm")

(define (check-error toplevel-list)
  (call-with-current-continuation
    (lambda (k)
      (fold
        (lambda (toplevel env)
          (guard
            (err
              ((tagged? v-error-tag err)
                (msg-print (untag err))
                (display "\n")
                (k #t))
              ((tagged? parse-error-tag err)
                (display "parse error: ")
                (write (untag err))
                (display "\n")
                (k #t)))
            (let ((res (eval-toplevel toplevel env)))
              (cadr res))))
        (env-init) toplevel-list)
      (display "no error occured:\n")
      (write toplevel-list)
      (display "\n")
      (exit 1))))