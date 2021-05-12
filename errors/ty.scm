(load "./mini.scm")


; +
(expect-error '(
  (+ "hello")
))

(expect-error '(
  (+ 1 "hello" 3)
))

; =
(expect-error '(
  (= #t #f)
))

; -
(expect-error '(
  (- 1 #f)
))

; car
(expect-error '(
  (car 1)
))

; cdr
(expect-error '(
  (cdr 1)
))

; set-car!
(expect-error '(
  (set-car! 1 2)
))

; set-cdr!
(expect-error '(
  (set-cdr! 1 2)
))