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


;;; prim (実行可能な式)
;; 定数式
(define prim-const-tag 'prim-const)
;; 手続き呼び出し
(define prim-call-tag 'prim-call)
(define (prim-call-tagged callee args expr)
  (assert (list? args))
  (tag prim-call-tag (list callee args expr)))
(define (prim-call.callee prim) (car prim))
(define (prim-call.args prim) (cadr prim))
(define (prim-call.expr prim) (caddr prim))
;; エラー
(define parse-error-tag 'parse-error)
(define (raise-parse-error expr)
  (raise (tag parse-error-tag expr)))

(define (parse-expr expr)
  (cond
    ((symbol? expr) expr)
    ((number? expr) (tag prim-const-tag expr))
    ((boolean? expr) (tag prim-const-tag expr))
    ((string? expr) (tag prim-const-tag expr))
    ((null? expr) (tag prim-const-tag expr))
    ((list? expr)
      (let
        ( (prims
            (map
              (lambda (e) (parse-expr e))
              expr)))
        (prim-call-tagged (car prims) (cdr prims) expr)))
    (else (raise-parse-error expr))))

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
;; 命令の戻り値
(define v-command-ret '())

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
    (env-bind-builtin 'display 1 #f
      (lambda (args)
        (v-display (car args))
        v-command-ret))
    (env-bind-builtin 'write 1 #f
      (lambda (args)
        (v-write (car args))
        v-command-ret))
    (env-bind-builtin 'flush 0 #f
      (lambda (args)
        (flush)
        v-command-ret))
  ))



;;; eval
(define (eval-prim prim top-env env)
  (cond
    ((symbol? prim)
      (cond
        ((env-lookup prim env) => env-bind.value)
        ((env-lookup prim top-env) => env-bind.value)
        (else
          (raise-v-error "unknown location: " (msg-w prim)))))
    ((tagged? prim-const-tag prim) (untag prim))
    ((tagged? prim-call-tag prim)
      (let*
        ( (prim (untag prim))
          (callee-prim (prim-call.callee prim))
          (args-prim (prim-call.args prim))
          (expr (prim-call.expr prim))
          (callee-v (eval-prim callee-prim top-env env))
          (argn (length args-prim))
          (args-v
            (map
              (lambda (p) (eval-prim p top-env env))
              args-prim)))
        (cond
          ; built-in procedure
          ((tagged? v-builtin-tag callee-v)
            (let*
              ( (builtin (untag callee-v))
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
                  (proc args-v))
                (raise-v-error
                  "wrong number of arguments: "
                  name " requires " argn-min ", but got " argn
                  "\n"
                  (msg-w expr)))))
          ; procedure ではない
          (else (raise-v-error "invalid application: " (msg-w expr))))))
    (else (error "eval-prim: fatal error:" prim))))


; returns (value top-env)
(define (eval-toplevel toplevel top-env)
  (cond
    ((match? '('define symbol _) toplevel)
      (let*
        ( (var (cadr toplevel))
          (expr (caddr toplevel))
          (prim (parse-expr expr))
          (value (eval-prim prim env-empty top-env)))
        (list '() (env-define var value top-env))))
    (else
      (let*
        ( (prim (parse-expr toplevel))
          (value (eval-prim prim top-env env-empty)))
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
                (guard
                  (condition
                    ((tagged? v-error-tag condition)
                      (display "error: ")
                      (msg-print (untag condition))
                      'error)
                    ((tagged? parse-error-tag condition)
                      (display "parse error: ")
                      (write (untag condition))
                      'error))
                  (tag 'ok
                    (eval-toplevel toplevel top-env)))))
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
