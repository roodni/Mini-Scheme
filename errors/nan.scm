(load "./mini.scm")

; =
(expect-error '(
  (= #t #f)
))

; -
(expect-error '(
  (- 1 #f)
))

; +
(expect-error '(
  (+ "hello")
))

(expect-error '(
  (+ 1 "hello" 3)
))