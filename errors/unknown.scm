(load "./check-error.scm")

(check-error '(
  (define x y)
  (define y 100)
))

(check-error '(
  (set! hao 123)
))

(check-error '(
  hello
))