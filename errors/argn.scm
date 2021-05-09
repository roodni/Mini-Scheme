(load "./check-error.scm")

(check-error '(
  ((lambda (x y . z) x) 1)
))

(check-error '(
  ((lambda (x) x) 1 2)
))

(check-error '(
  ((lambda (x) x))
))

(check-error '(
  (cons 1)
))

(check-error '(
  (cons 1 2 3)
))