(load "./check-error.scm")

; set!
(check-error '(
  (set! x 1 2)
))

(check-error '(
  (set! 0 1)
))

(check-error '(
  (set! x)
))

(check-error '(
  (set!)
))

; if
(check-error '(
  (if 1 2 3 4)
))

(check-error '(
  (if 1)
))

(check-error '(
  (if)
))

; define
(check-error '(
  (define (x y . 1) 1)
))

(check-error '(
  (define () 1)
))

(check-error '(
  (define)
))

; lambda
(check-error '(
  (lambda (a) (1 . 2))
))

(check-error '(
  (lambda (a))
))

(check-error '(
  (lambda 123 456)
))

; dotted list
(check-error '(
  (1 . 2)
))