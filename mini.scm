;;; util

;; tag
(define (tag id value)
  (cons id value))
(define (tagged? id obj)
  (and (pair? obj) (eq? id (car obj))))
(define (untag tagged)
  (cdr tagged))

;; match
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

;; assert
(define (assert x)
  (if (not x)
    (error "assertion failed")))

;; error message
(define msg-w-tag 'msg-w) ; use write
(define msg-v-tag 'msg-v) ; use v-write
(define (msg-w obj) (tag msg-w-tag obj))
(define (msg-v obj) (tag msg-v-tag obj))
(define (msg . l)
  (let check ((l l))
    (for-each
      (lambda (x)
        (assert
          (or (string? x)
              (number? x)
              (tagged? msg-w-tag x)
              (tagged? msg-v-tag x)
              (and (list? x) (check x)))))
      l))
  l)
(define (msg-print msg)
  (for-each
    (lambda (x)
      (cond
        ((tagged? msg-w-tag x) (write (untag x)))
        ((tagged? msg-v-tag x) (v-write (untag x)))
        ((list? x) (msg-print x))
        (else (display x))))
      msg))

;;; value
;; builtin
(define v-builtin-tag 'v-builtin)
(define (v-builtin-tagged name argn-min variadic proc)
  (assert (string? name))
  (assert (number? argn-min))
  (assert (boolean? variadic))
  (assert (procedure? proc))
  (tag v-builtin-tag (list name argn-min variadic proc)))
(define (v-builtin.name builtin) (car builtin))
(define (v-builtin.argn-min builtin) (cadr builtin))
(define (v-builtin.variadic? builtin) (caddr builtin))
(define (v-builtin.proc builtin) (cadddr builtin))
;; lambda
(define v-lambda-tag 'v-lambda)
;; pair
(define v-pair-tag 'v-pair)
;; error
(define v-error-tag 'v-error)
(define (raise-v-error . l)
  (raise (tag v-error-tag (msg l))))

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

(define (v->obj v)
  (cond
    ((number? v) v)
    ((boolean? v) v)
    ((string? v) v)
    ((null? v) v)
    ((tagged? v-pair-tag v)
      (let ((v (untag v)))
        (cons
          (v->obj (car v))
          (v->obj (cdr v)))))
    (else (error "cannot convert to obj:" v))))


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
            (if (number? v)
              (+ v total)
              (raise-v-error "number required, but got " (msg-v v))))
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
        (else
          (raise-v-error "unknown location: " (msg-w expr)))))
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
          ; built-in procedure
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
                (guard
                  (err
                    ((tagged? v-error-tag err)
                      (raise-v-error name ": " (untag err) "\n" (msg-w expr))))
                  (proc args))
                (raise-v-error
                  "wrong number of arguments: "
                  name " requires " argn-min ", but got " argn
                  "\n"
                  (msg-w expr)))))
          ; procedure ではない
          (else (raise-v-error "invalid application: " (msg-w expr))))))
    (else (raise-v-error "syntax error: " (msg-w expr)))))


; returns (value top-env)
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
    (let
      ( (toplevel
          (guard (condition
              ((<read-error> condition)
                (display "read error")
                'error) )
            (let ((input (read)))
              (if (eof-object? input)
                input
                (tag 'ok input))))))
      (cond
        ((eof-object? toplevel) 'bye)
        ((tagged? 'ok toplevel)
          (let*
            ( (toplevel (untag toplevel))
              (res
                (guard (condition
                    ((tagged? v-error-tag condition)
                      (display "error: ")
                      (msg-print (untag condition))
                      'error) )
                  (tag 'ok
                    (mini-eval-toplevel toplevel top-env)))))
            (cond
              ((tagged? 'ok res)
                (let*
                  ( (res (untag res))
                    (value (car res))
                    (top-env (cadr res)))
                  (v-write value)
                  (loop top-env)))
              (else (loop top-env)))))
        (else (loop top-env))))))
