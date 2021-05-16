(load "./mini.scm")


; dotted list
(expect-error '(
  (1 . 2)
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

; 内部定義
(expect-error '(
  (lambda ()
    (define x 1)
    (define y 2))
))
(expect-error '(
  (lambda ()
    (define x 1)
    (define y 2)
    . 1)
))

; quote
(expect-error '(
  (quote)
))
(expect-error '(
  (quote a b)
))
(expect-error '(
  '#(1 2)
))

; let
(expect-error '(
  (let)
))
(expect-error '(
  (let (x) x)
))
(expect-error '(
  (let ())
))
; let*
(expect-error '(
  (let*)
))
(expect-error '(
  (let* (x) x)
))
(expect-error '(
  (let* ())
))

; named let
(expect-error '(
  (let name)
))
(expect-error '(
  (let name (x) x)
))
(expect-error '(
  (let name ())
))