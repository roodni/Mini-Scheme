(load "./mini.scm")

(eval-toplevel-list '(
  (write (cons 1 (cons (cons 2 (cons "A" "B")) (cons 3 4))))
) (env-init))
(newline)

(eval-toplevel-list '(
  (write (cons 1 (cons 2 (cons 3 4))))
) (env-init))
(newline)

(eval-toplevel-list '(
  (write (cons 1 (cons 2 (cons 3 ()))))
) (env-init))
(newline)

(eval-toplevel-list '(
  (write (cons 1 2))
) (env-init))
(newline)

(eval-toplevel-list '(
  (display (lambda (a b) (+ a b)))
) (env-init))
(newline)

(eval-toplevel-list '(
  (display cons)
) (env-init))
(newline)

(eval-toplevel-list '(
  (display "hello")
) (env-init))
(newline)

(eval-toplevel-list '(
  (write "hello")
) (env-init))
(newline)

(eval-toplevel-list '(
  (display #t)
) (env-init))
(newline)

(eval-toplevel-list '(
  (display 123)
) (env-init))
(newline)

(eval-toplevel-list '(
  (display ())
) (env-init))
(newline)