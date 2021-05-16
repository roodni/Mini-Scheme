(load "./mini.scm")

(expect-error '(
  hello
))

(expect-error '(
  (set! hao 123)
))

(expect-error '(
  (define x y)
  (define y x)
))
(expect-error '(
  (define x y)
  (define y 100)
))
(expect-error '(
  ((lambda () (define x 1) ()))
  x
))

(expect-error '(
  (let ((a 1) (b a))
    (+ a b))
))