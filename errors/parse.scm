(load "./check-error.scm")

(check-error '(
  (define (x y . 1) 1)
))

(check-error '(
  (define () 1)
))

(check-error '(
  (define)
))

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