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