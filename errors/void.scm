(load "./mini.scm")

(expect-error '(
  (define (f)
    (define x y)
    (define y 1)
    x)
  (f)
))

(expect-error '(
  (define (f)
    (define x y)
    (define y x)
    x)
  (f)
))