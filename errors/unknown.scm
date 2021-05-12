(load "./mini.scm")

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