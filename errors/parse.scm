(load "./mini.scm")


; dotted list
(expect-parse-error '(
  (1 . 2)
))

; lambda
(expect-parse-error '(
  (lambda (a) 1 2 . 3)
))
(expect-parse-error '(
  (lambda (a) (1 . 2))
))
(expect-parse-error '(
  (lambda (a))
))
(expect-parse-error '(
  (lambda 123 456)
))

; define
(expect-parse-error '(
  (define (f) 1 . 2)
))
(expect-parse-error '(
  (define (f))
))
(expect-parse-error '(
  (define (x y . 1) 1)
))
(expect-parse-error '(
  (define () 1)
))
(expect-parse-error '(
  (define)
))

; if
(expect-parse-error '(
  (if 1 2 3 4)
))
(expect-parse-error '(
  (if 1)
))
(expect-parse-error '(
  (if)
))

; set!
(expect-parse-error '(
  (set! x 1 2)
))
(expect-parse-error '(
  (set! 0 1)
))
(expect-parse-error '(
  (set! x)
))
(expect-parse-error '(
  (set!)
))

; 内部定義
(expect-parse-error '(
  (lambda ()
    (define x 1)
    (define y 2))
))
(expect-parse-error '(
  (lambda ()
    (define x 1)
    (define y 2)
    . 1)
))

; quote
(expect-parse-error '(
  (quote)
))
(expect-parse-error '(
  (quote a b)
))
(expect-parse-error '(
  '#(1 2)
))

; let
(expect-parse-error '(
  (let)
))
(expect-parse-error '(
  (let (x) x)
))
(expect-parse-error '(
  (let ())
))
; let*
(expect-parse-error '(
  (let*)
))
(expect-parse-error '(
  (let* (x) x)
))
(expect-parse-error '(
  (let* ())
))

; named let
(expect-parse-error '(
  (let name)
))
(expect-parse-error '(
  (let name (x) x)
))
(expect-parse-error '(
  (let name ())
))

; and
(expect-parse-error '(
  (and 1 . 2)
))
; or
(expect-parse-error '(
  (or . "fuga")
))

; cond
(expect-parse-error '(
  (cond (else))
))
(expect-parse-error '(
  (cond (else 1) (#t 1))
))
(expect-parse-error '(
  (cond (#t))
))
(expect-parse-error '(
  (cond ())
))

; guard
(expect-parse-error '(
  (guard)
))
(expect-parse-error '(
  (guard ((else 1)) 1)
))
(expect-parse-error '(
  (guard (c (else 1)))
))