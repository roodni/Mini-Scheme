(load "./mini.scm")

; set!
(expect-error '(
  (set! x 1 2)
))

(expect-error '(
  (set! 0 1)
))

(expect-error '(
  (set! x)
))

(expect-error '(
  (set!)
))

; if
(expect-error '(
  (if 1 2 3 4)
))

(expect-error '(
  (if 1)
))

(expect-error '(
  (if)
))

; define
(expect-error '(
  (define (f) 1 . 2)
))

(expect-error '(
  (define (f))
))

(expect-error '(
  (define (x y . 1) 1)
))

(expect-error '(
  (define () 1)
))

(expect-error '(
  (define)
))

; lambda
(expect-error '(
  (lambda (a) 1 2 . 3)
))

(expect-error '(
  (lambda (a) (1 . 2))
))

(expect-error '(
  (lambda (a))
))

(expect-error '(
  (lambda 123 456)
))

; dotted list
(expect-error '(
  (1 . 2)
))