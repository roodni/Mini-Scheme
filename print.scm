(load "./mini.scm")

(eval-toplevel-list '(
  (display "hello")
  (newline)

  (write "hello")
  (newline)

  (display #t)
  (newline)

  (display 123)
  (newline)

  (display ())
  (newline)

  (display (lambda (a b) (+ a b)))
  (newline)

  (display cons)
  (newline)

  (write (cons 1 (cons (cons 2 (cons "A" "B")) (cons 3 4))))
  (newline)

  (write (cons 1 (cons 2 (cons 3 4))))
  (newline)

  (write (cons 1 (cons 2 (cons 3 ()))))
  (newline)

  (write (cons 1 2))
  (newline)

  (define l (cons 1 (cons 2 ())))
  (set-car! (cdr l) l)
  (write l)
  (newline)

  (define l (cons 1 (cons 2 ())))
  (set-cdr! (cdr l) l)
  (write l)
  (newline)

  (guard
    (e (else (write e)))
    (< "hello" "world"))
  (newline)

) (env-init))