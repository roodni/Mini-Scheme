(load "./mini.scm")

; cons
(expect-error '(
  (cons 1)
))

(expect-error '(
  (cons 1 2 3)
))

; =
(expect-error '(
  (=)
))
(expect-error '(
  (= 0)
))

; <
(expect-error '(
  (<)
))
(expect-error '(
  (< 0)
))

; >
; <
(expect-error '(
  (>)
))
(expect-error '(
  (> 0)
))

; -
(expect-error '(
  (-)
))

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

; car
(expect-error '(
  (define p (cons 1 2))
  (car p p)
))

(expect-error '(
  (car)
))

; cdr
(expect-error '(
  (define p (cons 1 2))
  (cdr p p)
))

(expect-error '(
  (cdr)
))

; set-car!
(expect-error '(
  (define p (cons 1 2))
  (set-car! p)
))

; set-car!
(expect-error '(
  (define p (cons 1 2))
  (set-car! p p p)
))

; set-cdr!
(expect-error '(
  (define p (cons 1 2))
  (set-cdr! p)
))

; set-cdr!
(expect-error '(
  (define p (cons 1 2))
  (set-cdr! p p p)
))