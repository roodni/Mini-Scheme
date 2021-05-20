(load "./mini.scm")

(expect-error '(
  (/ 0)
))

(expect-error '(
  (/ 1 2 0 3)
))