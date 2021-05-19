(load "./mini.scm")

(define (mini-test expected toplevel-list)
  (let*
    ( (res (eval-toplevel-list toplevel-list (env-init)))
      (obj (v->obj (car res))) )
    (cond
      ((equal? expected obj)
        (display ".")
        (flush))
      (else (error "fail" expected obj)))))

;; built-in procedure
; eq?
(mini-test #f '( (eq? 'a 'b) ))
(mini-test #f '( (eq? 2 2.0) ))
(mini-test #f '(
  (eq? (cons 1 2) (cons 1 2))
))
(mini-test #f '(
  (eq? (lambda () 1) (lambda () 2))
))
(mini-test #f '( (eq? #f 'nil) ))
(mini-test #t '( (eq? 'a 'a) ))
(mini-test #f '(
  (eq? (list 'a) (list 'a))
))
(mini-test #t '( (eq? '() '()) ))
(mini-test #t '( (eq? car car) ))
(mini-test #t '(
  (let ((x '(a)))
    (eq? x x))
))
(mini-test #t '(
  (let ((p (lambda (x) x)))
    (eq? p p))
))

; not
(mini-test #f '( (not #t) ))
(mini-test #t '( (not #f) ))
(mini-test #f '( (not 3) ))
(mini-test #f '( (not (list)) ))

; symbol->string
(mini-test "mini" '(
  (symbol->string 'mini)
))

; procedure?
(mini-test #t '(
  (procedure? procedure?)
))
(mini-test #t '(
  (procedure? (lambda (x) x))
))
(mini-test #f '(
  (procedure? 2)
))

; boolean?
(mini-test #t '( (boolean? #f) ))
(mini-test #t '( (boolean? #t) ))
(mini-test #f '( (boolean? 1) ))

; number?
(mini-test #t '( (number? 1) ))
(mini-test #f '( (number? "1") ))

; string?
(mini-test #t '( (string? "mini") ))
(mini-test #f '( (string? 0) ))

; pair?
(mini-test #t '(
  (pair? (cons 1 2))
))
(mini-test #f '( (pair? '()) ))

; null?
(mini-test #t '( (null? '()) ))
(mini-test #f '(
  (null? (cons 1 2))
))

; set-car! set-cdr!
(mini-test 2 '(
  (define l (cons 1 (cons 3 ())))
  (set-cdr! (cdr l) l)
  (-  (car (cdr (cdr (cdr (cdr (cdr l))))))
      (car (cdr (cdr l))))
))

(mini-test v-command-ret '(
  (define p (cons 1 2))
  (set-car! p 200)
))
(mini-test v-command-ret '(
  (define p (cons 1 2))
  (set-cdr! p 200)
))

(mini-test '(1 . 200) '(
  (define p (cons 1 2))
  (set-cdr! p 200)
  p
))
(mini-test '(100 . 2) '(
  (define p (cons 1 2))
  (set-car! p 100)
  p
))

; car cdr
(mini-test 2 '(
  (define a (cons 1 (cons 2 (cons 3 ()))))
  (car (cdr a))
))

; cadr
(mini-test 2 '(
  (cadr (list 1 2 3))
))

; =
(mini-test #f '( (= 1 1 1 2) ))
(mini-test #f '( (= 1 0 1 1) ))
(mini-test #t '( (= 1 1 1 1) ))
(mini-test #f '( (= 1 2) ))
(mini-test #t '( (= 1 1) ))

; <
(mini-test #t '( (< 1 2) ))
(mini-test #f '( (< 1 1) ))
(mini-test #f '( (< 1 0) ))
(mini-test #t '( (< 1 2 3) ))
(mini-test #f '( (< 1 2 1) ))

; >
(mini-test #f '( (> 1 2) ))
(mini-test #f '( (> 2 2) ))
(mini-test #t '( (> 3 2) ))
(mini-test #t '( (> 4 3 2 1) ))
(mini-test #f '( (> 4 3 0 1) ))

; >=
(mini-test #f '( (>= 1 2) ))
(mini-test #t '( (>= 2 2) ))
(mini-test #t '( (>= 3 2) ))
(mini-test #t '( (>= 2 1 1 1) ))
(mini-test #f '( (>= 4 3 0 1) ))

; -
(mini-test -7 '(
  (- 1 3 5)
))
(mini-test 2 '(
  (- 3 1)
))
(mini-test -10 '(
  (- 10)
))

; +
(mini-test 6 '(
  (+ 1 2 3)
))
(mini-test 0 '(
  (+)
))

; *
(mini-test 48 '(
  (* 2 4 6)
))
(mini-test 1 '(
  (*)
))

; cons
(mini-test (list 1 2 3) '(
  (cons 1 (cons 2 (cons 3 ())))
))

; list
(mini-test '() '(
  (list)
))
(mini-test '(1 2 3 4) '(
  (list 1 2 3 4)
))

; fold
(mini-test 23 '(
  (fold + 0 '(3 1 4 1 5 9))
))
(mini-test '(e d c b a) '(
  (fold cons '() '(a b c d e))
))

; memq
(mini-test '(a b c) '(
  (memq 'a '(a b c))
))
(mini-test '(b c) '(
  (memq 'b '(a b c))
))
(mini-test #f '(
  (memq 'a '(d b c))
))
(mini-test #f '(
  (memq (list 'a) '(b (a) c))
))

; list?
(mini-test #t '( (list? '()) ))
(mini-test #t '(
  (list? (list 1 2 3))
))
(mini-test #f '(
  (list? (cons 1 2))
))
(mini-test #f '(
  (define l '(1 2))
  (set-cdr! (cdr l) l)
  (list? l)
))

;; other
; load
(mini-test '46 '(
  (load "./samples/hoge.scm")
  (f)
))

; guard
(mini-test 'ok '(
  (guard
    (c (else 'error))
  'ok)
))
(mini-test 'greater '(
  (guard
    (c
      ((> c 2) 'greater)
      ((< c 2) 'less)
      (else 'equal))
    (raise 3))
))
(mini-test 'less '(
  (guard
    (c
      ((> c 2) 'greater)
      ((< c 2) 'less)
      (else 'equal))
    (raise 1))
))
(mini-test 'equal '(
  (guard
    (c
      ((> c 2) 'greater)
      ((< c 2) 'less)
      (else 'equal))
    (raise 2))
))
(mini-test 'equal2 '(
  (guard
    (c ((= c 2) 'equal2))
    (guard
      (c
        ((> c 2) 'greater)
        ((< c 2) 'less))
      (raise 2)))
))
(mini-test 'e '(
  (guard
    (c (else 'e))
  (< 1 0+1i))
))
(mini-test 2 '(
  (guard
    (_ (#f 1) (#t 2) (#t 3) (else 4))
  (raise 'e))
))

; cond
(mini-test 'greater '(
  (cond
    ((> 3 2) 'greater)
    ((< 3 2) 'less))
))
(mini-test 'equal '(
  (cond
    ((> 3 3) 'greater)
    ((< 3 3) 'less)
    (else 'equal))
))
(mini-test 1 '(
  (cond
    (#t 1)
    (#t 2)
    (#t 3))
))
(mini-test v-command-ret '(
  (cond (#f 1) (#f 2))
))
(mini-test '((6 1 3) (-5 -2)) '(
  (let loop
    ( (numbers '(3 -2 1 6 -5))
      (nonneg '())
      (neg '()) )
    (cond
      ((null? numbers) (list nonneg neg))
      ((>= (car numbers) 0)
        (loop
          (cdr numbers)
          (cons (car numbers) nonneg)
          neg))
      ((< (car numbers) 0)
        (loop
          (cdr numbers)
          nonneg
          (cons (car numbers) neg)))))
))

; or
(mini-test #t '(
  (or (= 2 2) (> 2 1))
))
(mini-test #t '(
  (or (= 2 2) (< 2 1))
))
(mini-test #f '(
  (or #f #f #f)
))
(mini-test '(b c) '(
  (or '(b c) (car #t))
))

; and
(mini-test #t '(
  (and (= 2 2) (> 2 1))
))
(mini-test #f '(
  (and (= 2 2) (< 2 1))
))
(mini-test '(f g) '(
  (and 1 2 'c '(f g))
))
(mini-test #t '(
  (and)
))
(mini-test 0 '(
  (define v 0)
  (and #f (set! v 1))
  v
))

; let*
(mini-test 70 '(
  (let ((x 2) (y 3))
    (let*
      ( (x 7)
        (z (+ x y)) )
      (* z x)))
))

; named let
(mini-test 55 '(
  (let loop ((n 10))
    (if (= n 0)
      0
      (+ n (loop (- n 1)))))
))

; let
(mini-test 6 '(
  (let ((x 2) (y 3))
    (* x y))
))
(mini-test 35 '(
  (let ((x 2) (y 3))
    (let
      ( (x 7)
        (z (+ x y)) )
      (* z x)))
))
(mini-test 1 '(
  (let () 1)
))

; quote
(mini-test '((1 2) (3 4)) '(
  '((1 2) (3 4))
))
(mini-test 'a '(
  (quote a)
))
(mini-test '(+ 1 2) '(
  (quote (+ 1 2))
))
(mini-test "abc" '(
  '"abc"
))
(mini-test 145932 '(
  '145932
))
(mini-test #t '(
  '#t
))
(mini-test '() '(
  '()
))

; 内部定義
(mini-test 45 '(
  (define (f x)
    (define foo (lambda (y) (bar x y)))
    (define bar (lambda (a b) (+ (* a b) a)))
    (foo (+ x 3)))
  (f 5)
))
(mini-test 102 '(
  ( (lambda (a b)
      (define a 100)
      (+ a b))
    1 2 )
))
(mini-test 123 '(
  (define (f)
    (define a 1)
    (define b 2)
    (define c 3)
    (+ (* 100 a) (* 10 b) c))
  (f)
))

; set!
(mini-test v-command-ret '(
  (define x 0)
  (set! x 1)
))
(mini-test '(3 2 1) '(
  (define st ())
  (define (push v)
    (set! st (cons v st)))
  (push 1)
  (push 2)
  (push 3)
  st
))
(mini-test 200 '(
  (define x 12345)
  (define (f x)
    (set! x 200)
    x)
  (f 100)
))
(mini-test 2 '(
  (define x 1)
  (set! x 2)
  x
))

; if
(mini-test '("t") '(
  (define st ())
  (define (push v)
    (set! st (cons v st)))
  (if #f (push "f"))
  (if #t (push "t"))
  st
))

(mini-test 1 '(
  (if #t 1 2)
))
(mini-test 2 '(
  (if #f 1 2)
))

(mini-test "hello" '(
  (if 1 "hello")
))
(mini-test v-command-ret '(
  (if #f "hello")
))

; define (手続きの定義)
(mini-test 123 '(
  (define n 0)
  (define (f i)
    (set! n (+ n n n n n n n n n n i)))
  (define (g)
    (f 1) (f 2) (f 3) n)
  (g)
))

(mini-test 100 '(
  (define (x) y)
  (define y 100)
  ((lambda (y) (x)) 200)
))
(mini-test 200 '(
  (define y 100)
  ((lambda (y) y) 200)
))
(mini-test 100 '(
  (define (x) y)
  (define y 100)
  (x)
))

(mini-test #f '(
  (define (even? x)
    (if (= x 0) #t (odd? (- x 1))))
  (define (odd? x)
    (if (= x 0) #f (even? (- x 1))))
  (even? 1001)
))
(mini-test #t '(
  (define (even? x)
    (if (= x 0) #t (odd? (- x 1))))
  (define (odd? x)
    (if (= x 0) #f (even? (- x 1))))
  (even? 1000)
))

(mini-test '(1 2 . (3 4 5)) '(
  (define (f x y . z)
    (cons x (cons y z)))
  (f 1 2 3 4 5)
))
(mini-test '(1 2 3 4) '(
  (define (f . l) l)
  (f 1 2 3 4)
))
(mini-test 3 '(
  (define (f a b) (+ a b))
  (f 1 2)
))


; lambda
(mini-test '(1 2 3 4 5) '(
  ((lambda l l) 1 2 3 4 5)
))
(mini-test '(1 2 . (3 4 5)) '(
  ((lambda (x y . z)
      (cons x (cons y z)))
    1 2 3 4 5)
))

(mini-test 123 '(
  (define s
    (lambda (x) (lambda (y) (lambda (z)
      ((x z) (y z))))))
  (define k
    (lambda (x) (lambda (y) x)))
  (((s k) k) 123)
))

(mini-test 300 '(
  ((lambda () 100 200 300))
))
(mini-test 3 '(
  ((lambda (a b) (+ a b)) 1 2)
))

; define (単純)
(mini-test v-command-ret '(
  (define x 0)
))
(mini-test 456 '(
  (define hello 123)
  (define hello 456)
  hello
))
(mini-test 123 '(
  (define hello 123)
  hello
))

; const
(mini-test #t '(
  #t
))
(mini-test "hello" '(
  "hello"
))
(mini-test 1 '(
  1
))
(mini-test '() '(
  ()
))