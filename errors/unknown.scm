(load "./mini.scm")

(expect-error '(
  ((lambda () (define x 1) ()) )
  x
))

(expect-error '(
  (define x y)
  (define y 100)
))

(expect-error '(
  (define x y)
  (define y 100)
))

(expect-error '(
  (set! hao 123)
))

(expect-error '(
  hello
))