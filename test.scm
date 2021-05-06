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