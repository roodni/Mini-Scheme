(load "./mini.scm")

(define (mini-test expected toplevel-list)
  (let*
    ( (res
        (fold
          (lambda (toplevel res)
            (define env (cadr res))
            (eval-toplevel toplevel env))
          (list #t (env-init)) toplevel-list))
      (obj (v->obj (car res))) )
    (if (equal? expected obj)
      (display ".")
      (error "fail" expected obj))))

; todo: 評価順序のテスト

; (mini-test #t '(
;   (define (even? x)
;     (if (= x 0) #t (odd? (- x 1))))
;   (define (odd? x)
;     (if (= x 0) #f (even? (- x 1))))
;   (even? 1000000000)
; ))

; define (手続きの定義)
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

(mini-test 3 '(
  ((lambda (a b) (+ a b)) 1 2)
))

; built-in =
(mini-test #f '(
  (= 1 1 1 2)
))

(mini-test #f '(
  (= 1 0 1 1)
))

(mini-test #t '(
  (= 1 1 1 1)
))

(mini-test #f '(
  (= 1 2)
))

(mini-test #t '(
  (= 1 1)
))

; built-in -
(mini-test -7 '(
  (- 1 3 5)
))

(mini-test 2 '(
  (- 3 1)
))

(mini-test -10 '(
  (- 10)
))

; built-in +
(mini-test 6 '(
  (+ 1 2 3)
))

(mini-test 0 '(
  (+)
))

; built-in cons
(mini-test (list 1 2 3) '(
  (cons 1 (cons 2 (cons 3 ())))
))

; define (単純)
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