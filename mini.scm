;;; util
(define (match? pat obj)
  (cond
    ((eq? pat '_) #t)
    ((eq? pat 'symbol) (symbol? obj))
    ((null? pat) (null? obj))
    ((and
        (list? pat)
        (eq? (car pat) 'quote)
        (= (length pat) 2))
      (equal? (cadr pat) obj))
    ((pair? pat)
      (and
        (pair? obj)
        (match? (car pat) (car obj))
        (match? (cdr pat) (cdr obj))))
    (else (error "match?: invalid pattern:" pat))))

;;; env
(define mini-env-empty '())

(define (mini-env-lookup var env)
  (assoc var env))

(define (mini-env-bind-value bind) (cdr bind))

(define (mini-env-extend var value env)
  (cons (cons var value) env))

(define (mini-env-define var value env)
  (let ((bind (mini-env-lookup var env)))
    (if bind
      (begin
        (set-cdr! bind value)
        env)
      (mini-env-extend var value env))))

;;; eval
(define (mini-eval-expr expr top-env env)
  (cond
    ((symbol? expr)
      (cond
        ((mini-env-lookup expr env) => mini-env-bind-value)
        ((mini-env-lookup expr top-env) => mini-env-bind-value)
        (else (error "unknown location:" expr))))
    ((number? expr) expr)
    ((boolean? expr) expr)
    ((string? expr) expr)
    (else (error "syntax error:" expr))))

;; returns (value top-env)
(define (mini-eval-toplevel toplevel top-env)
  (cond
    ((match? '('define symbol _) toplevel)
      (let*
        ( (var (cadr toplevel))
          (expr (caddr toplevel))
          (value (mini-eval-expr expr mini-env-empty top-env)))
        (list '() (mini-env-define var value top-env))))
    (else
      (let ((value (mini-eval-expr toplevel top-env mini-env-empty)))
        (list value top-env)))))


;;; repl
(define (mini-repl)
  (display "mini-scheme intepreter\n")
  (let loop ((top-env mini-env-empty))
    (display "\n> ")
    (flush)
    (let ((toplevel (read)))
      (if (not (eof-object? toplevel))
        (let*
          ( (res
              (guard (condition
                  (else
                    (display "fatal error: ")
                    (display condition)
                    'error))
                (list 'ok (mini-eval-toplevel toplevel top-env)))))
          (cond
            ((match? '('ok _) res)
              (let*
                ( (res (cadr res))
                  (value (car res))
                  (top-env (cadr res)))
                (write value)
                (loop top-env)))
            (else (loop top-env))))))))
