;;; util

(define (tag id value)
  (cons id value))
(define (tagged? id obj)
  (and (pair? obj) (eq? id (car obj))))
(define (untag tagged)
  (cdr tagged))

(define (prefixed-list? prefix len obj)
  (and
    (list? obj)
    (= len (length obj))
    (eq? prefix (car obj))))

(define (match? pat obj)
  (cond
    ((eq? pat '_) #t)
    ((eq? pat 'symbol) (symbol? obj))
    ((eq? pat 'string) (string? obj))
    ((null? pat) (null? obj))
    ((prefixed-list? 'quote 2 pat)
      (equal? (cadr pat) obj))
    ((prefixed-list? '* 3 pat)
      (let
        ( (pat-loop (cadr pat))
          (pat-end (caddr pat)))
        (or
          (match? pat-end obj)
          (and
            (pair? obj)
            (match? pat-loop (car obj))
            (match? pat (cdr obj))))))
    ((pair? pat)
      (and
        (pair? obj)
        (match? (car pat) (car obj))
        (match? (cdr pat) (cdr obj))))
    (else (error "match?: invalid pattern:" pat))))



;;; error
(define mini-error-tag 'mini-error)

(define (raise-mini-error msg expr)
  (raise (tag mini-error-tag (list msg expr))))

(define (mini-error? obj)
  (and
    (tagged? mini-error-tag obj)
    (match? '(_ _) (untag obj))))

(define (mini-error.msg err)
  (car (untag err)))

(define (mini-error.expr err)
  (cadr (untag err)))


;;; value
(define v-builtin-tag 'builtin)
(define (v-builtin-tagged name argn-min variadic proc)
  (tag v-builtin-tag (list name argn-min variadic proc)))
(define (v-builtin.name builtin) (car builtin))
(define (v-builtin.argn-min builtin) (cadr builtin))
(define (v-builtin.variadic? builtin) (caddr builtin))
(define (v-builtin.proc builtin) (cadddr builtin))

(define v-lambda-tag 'lambda)

(define v-pair-tag 'pair)

(define (v-print base-print is-cdr v)
  (cond
    ((number? v) (base-print v))
    ((boolean? v) (base-print v))
    ((string? v) (base-print v))
    ((null? v) (base-print v))
    ((tagged? v-builtin-tag v)
      (let*
        ( (v (untag v))
          (name (v-builtin.name v)))
        (display "[built-in ")
        (display name)
        (display "]")))
    ((tagged? v-pair-tag v)
      (let*
        ( (v (untag v))
          (a (car v))
          (d (cdr v)))
        (if (not is-cdr) (display "("))
        (v-print base-print #f a)
        (cond
          ((null? d) 'do-nothing)
          ((tagged? v-pair-tag d)
            (display " ")
            (v-print base-print #t d))
          (else
            (display " . ")
            (v-print base-print #t d)))
        (if (not is-cdr) (display ")"))))
    (else
      (display "[not-implemented: ")
      (write v)
      (display "]"))))

(define (v-display v) (v-print display #f v))
(define (v-write v) (v-print write #f v))

;;; env
(define (env-bind var value) (cons var value))

(define (env-bind.value bind) (cdr bind))

(define (env-lookup var env)
  (assoc var env))

(define (env-extend var value env)
  (cons (cons var value) env))

(define (env-define var value env)
  (let ((bind (env-lookup var env)))
    (if bind
      (begin
        (set-cdr! bind value)
        env)
      (env-extend var value env))))

(define env-empty '())

(define (env-init)
  (define (env-bind-builtin var argn-min variadic proc)
    (cons
      var
      (v-builtin-tagged
        (symbol->string var)
        argn-min variadic proc)))
  (list
    (env-bind-builtin 'cons 2 #f
      (lambda (args)
        (define a (car args))
        (define d (cadr args))
        (tag v-pair-tag (cons a d))))
    (env-bind-builtin '+ 0 #t
      (lambda (args)
        (fold
          (lambda (v total)
            (if (number? v) (+ v total) (error "not a number")))
          0 args)))
  ))


;;; eval
(define (mini-eval-expr expr top-env env)
  (cond
    ((number? expr) expr)
    ((boolean? expr) expr)
    ((string? expr) expr)
    ((null? expr) expr)
    ((symbol? expr)
      (cond
        ((env-lookup expr env) => env-bind.value)
        ((env-lookup expr top-env) => env-bind.value)
        (else (raise-mini-error "unknown location:" expr))))
    ((list? expr) ; 空リストは上で捕捉されるので空リストでない
      (let*
        ( (values
            (map
              (lambda (e) (mini-eval-expr e top-env env))
              expr))
          (callee (car values))
          (args (cdr values))
          (argn (length args)))
        (cond
          ((tagged? v-builtin-tag callee)
            (let*
              ( (builtin (untag callee))
                (name (v-builtin.name builtin))
                (argn-min (v-builtin.argn-min builtin))
                (variadic (v-builtin.variadic? builtin))
                (proc (v-builtin.proc builtin)))
              (if
                (or
                  (= argn-min argn)
                  (and variadic (< argn-min argn)))
                (proc args); あとでエラー処理を書く
                (raise-mini-error
                  (string-append
                    "wrong number of arguments: "
                    name " requires "
                    (number->string argn-min) ", but got "
                    (number->string argn))
                  expr))))
          (else (raise-mini-error "invalid application:" expr)))
      )
    )
    (else (raise-mini-error "syntax error:" expr))))


;; returns (value top-env)
(define (mini-eval-toplevel toplevel top-env)
  (cond
    ((match? '('define symbol _) toplevel)
      (let*
        ( (var (cadr toplevel))
          (expr (caddr toplevel))
          (value (mini-eval-expr expr env-empty top-env)))
        (list '() (env-define var value top-env))))
    (else
      (let ((value (mini-eval-expr toplevel top-env env-empty)))
        (list value top-env)))))


;;; repl
(define (mini-repl)
  (display "mini-scheme intepreter\n")
  (let loop ((top-env (env-init)))
    (display "\n> ")
    (flush)
    (let ((toplevel (read)))
      (if (not (eof-object? toplevel))
        (let*
          ( (res
              (guard
                (condition
                  ((mini-error? condition)
                    (display (mini-error.msg condition))
                    (display " ")
                    (write (mini-error.expr condition))
                    'error)
                )
                (tag 'ok (mini-eval-toplevel toplevel top-env)))))
          (cond
            ((tagged? 'ok res)
              (let*
                ( (res (untag res))
                  (value (car res))
                  (top-env (cadr res)))
                (v-write value)
                (loop top-env)))
            (else (loop top-env))))))))
