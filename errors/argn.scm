(load "./mini.scm")

; lambda
(expect-error '(
  ((lambda (x y . z) x) 1)
))

(expect-error '(
  ((lambda (x) x) 1 2)
))

(expect-error '(
  ((lambda (x) x))
))

; built-in =
(expect-error '(
  (=)
))
(expect-error '(
  (= 0)
))

; built-in -
(expect-error '(
  (-)
))

; built-in cons
(expect-error '(
  (cons 1)
))

(expect-error '(
  (cons 1 2 3)
))