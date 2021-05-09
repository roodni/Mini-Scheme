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

; todo: 可変長引数のテスト 評価順序のテスト

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

(mini-test 6 '(
  (+ 1 2 3)
))

(mini-test 0 '(
  (+)
))

(mini-test (list 1 2 3) '(
  (cons 1 (cons 2 (cons 3 ())))
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

(mini-test #t '(
  #t
))

(mini-test "hello" '(
  "hello"
))

(mini-test 1 '(
  1
))