(load "./mini.scm")

(expect-error '(
  (letrec ((a b) (b a))
    a)
))

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