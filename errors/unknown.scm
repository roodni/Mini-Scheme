(load "./check-error.scm")

(check-error '(
  (set! hao 123)
))

(check-error '(
  hello
))