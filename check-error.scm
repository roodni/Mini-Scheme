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
                (k #t)))
            (let ((res (mini-eval-toplevel toplevel env)))
              (cadr res))))
        (env-init) toplevel-list)
      (display "no error occured\n")
      (exit 1))))