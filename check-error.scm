(load "./mini.scm")

(define (check-error toplevel-list)
  (guard
    (err
      ((tagged? v-error-tag err)
        (msg-print (untag err))
        (newline))
      ((tagged? parse-error-tag err)
        (display "syntax error: ")
        (write (untag err))
        (newline)))
    (eval-toplevel-list toplevel-list (env-init))
    (display "no error occured:\n")
    (write toplevel-list)
    (newline)
    (exit 1)))