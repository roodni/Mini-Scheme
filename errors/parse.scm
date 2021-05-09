(load "./check-error.scm")

(check-error '(
  (lambda (a) (1 . 2))
))

(check-error '(
  (lambda (a))
))

(check-error '(
  (lambda 123 456)
))

(check-error '(
  (1 . 2)
))