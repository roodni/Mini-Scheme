(load "./check-error.scm")

; =
(check-error '(
  (= #t #f)
))

; -
(check-error '(
  (- 1 #f)
))

; +
(check-error '(
  (+ "hello")
))

(check-error '(
  (+ 1 "hello" 3)
))