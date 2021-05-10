;;; utils
;; assert
(define (assert x)
  (if (not x)
    (error "assertion failed")))

;; match
(define (prefixed-list? prefix obj)
  (and
    (list? obj)
    (eq? prefix (car obj))))

(define (match? pat obj)
  (cond
    ((eq? pat '_) #t)
    ((eq? pat 'symbol) (symbol? obj))
    ((eq? pat 'string) (string? obj))
    ((null? pat) (null? obj))
    ((prefixed-list? 'quote pat)
      (assert (= 2 (length pat)))
      (equal? (cadr pat) obj))
    ((prefixed-list? '* pat)
      (assert (= 3 (length pat)))
      (let
        ( (pat-loop (cadr pat))
          (pat-end (caddr pat)))
        (or
          (match? pat-end obj)
          (and
            (pair? obj)
            (match? pat-loop (car obj))
            (match? pat (cdr obj))))))
    ((prefixed-list? 'or pat)
      (assert (= 3 (length pat)))
      (let
        ( (pat-l (cadr pat))
          (pat-r (caddr pat)))
        (or (match? pat-l obj)
            (match? pat-r obj))))
    ((pair? pat)
      (and
        (pair? obj)
        (match? (car pat) (car obj))
        (match? (cdr pat) (cdr obj))))
    (else (assert #f))))

;; tag
(define (tag id value)
  (cons id value))
(define (tagged? id obj)
  (and (pair? obj) (eq? id (car obj))))
(define (untag tagged)
  (cdr tagged))

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
;; 手続き
(define prim-lambda-tag 'prim-lambda)
(define (prim-lambda-tagged arg mid-prims last-prim)
  (assert (list? mid-prims))
  (tag prim-lambda-tag (list arg mid-prims last-prim)))
(define (prim-lambda.arg prim) (car prim))
(define (prim-lambda.mid-prims prim) (cadr prim))
(define (prim-lambda.last-prim prim) (caddr prim))

;; expr -> prim
(define parse-error-tag 'parse-error)
(define (raise-parse-error expr)
  (raise (tag parse-error-tag expr)))

; returns (var prim)
(define (parse-define def)
  (cond
    ((match? '('define symbol _) def)
      (list (cadr def) (parse-expr (caddr def))))
    ((match? '('define (symbol . (* symbol (or () symbol))) . _) def)
      (let*
        ( (var-and-arg (cadr def))
          (var (car var-and-arg))
          (arg (cdr var-and-arg))
          (body-parsed (parse-body (cddr def) def))
          (mid-prims (car body-parsed))
          (last-prim (cadr body-parsed)))
        (list var (prim-lambda-tagged arg mid-prims last-prim))))
    (else (raise-parse-error def))))

; returns (mid-prims last-prim)
(define (parse-body body error-expr)
  (let parse-exprs
    ( (mid-prims-rev (list))
      (exprs body))
    (cond
      ((match? '(_) exprs)
        (let*
          ( (last-expr (car exprs))
            (last-prim (parse-expr last-expr))
            (mid-prims (reverse mid-prims-rev)))
          (list mid-prims last-prim)))
      ((match? '(_ . _) exprs)
        (let*
          ( (mid-expr (car exprs))
            (mid-prim (parse-expr mid-expr)))
          (parse-exprs
            (cons mid-prim mid-prims-rev)
            (cdr exprs))))
      (else (raise-parse-error error-expr)))))

(define (parse-expr expr)
  (cond
    ((symbol? expr) expr)
    ((number? expr) (tag prim-const-tag expr))
    ((boolean? expr) (tag prim-const-tag expr))
    ((string? expr) (tag prim-const-tag expr))
    ((null? expr) (tag prim-const-tag expr))
    ((match? '('lambda . _) expr)
      (cond
        ((match? '((* symbol (or () symbol)) . _) (cdr expr))
          (let*
            ( (arg (cadr expr))
              (body-parsed (parse-body (cddr expr) expr))
              (mid-prims (car body-parsed))
              (last-prim (cadr body-parsed)))
            (prim-lambda-tagged arg mid-prims last-prim)))
        (else (raise-parse-error expr))))
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
(define (v-lambda-tagged env arg mid-prims last-prim)
  (tag v-lambda-tag (list env arg mid-prims last-prim)))
(define (v-lambda.env lam) (car lam))
(define (v-lambda.arg lam) (cadr lam))
(define (v-lambda.argn-min lam)
  (let count ((arg (v-lambda.arg lam)) (n 0))
    (cond
      ((pair? arg) (count (cdr arg) (+ n 1)))
      (else n))))
(define (v-lambda.mid-prims lam) (caddr lam))
(define (v-lambda.last-prim lam) (cadddr lam))
;; pair
(define v-pair-tag 'v-pair)
(define (v-pair-tagged kar kdr)
  (tag v-pair-tag (cons kar kdr)))
(define (v-pair.car p) (car p))
(define (v-pair.cdr p) (cdr p))
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
    ((tagged? v-lambda-tag v)
      (let*
        ( (lam (untag v))
          (arg (v-lambda.arg lam)))
        (display "[lambda ")
        (write arg)
        (display " ...]")))
    ((tagged? v-pair-tag v)
      (let*
        ( (v (untag v))
          (a (v-pair.car v))
          (d (v-pair.cdr v)))
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
    ((tagged? v-error-tag v)
      (display "[error]"))
    (else
      (display "[?: ")
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
          (v->obj (v-pair.car v))
          (v->obj (v-pair.cdr v)))))
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
  (define (raise-nan-error v)
    (raise-v-error "number required, but got " (msg-v v)))
  (define (expect-number-list vl)
    (for-each
      (lambda (x)
        (if (not (number? x)) (raise-nan-error x)))
      vl))
  (list
    (env-bind-builtin 'cons 2 #f
      (lambda (args)
        (define a (car args))
        (define d (cadr args))
        (v-pair-tagged a d)))
    (env-bind-builtin '+ 0 #t
      (lambda (args)
        (expect-number-list args)
        (fold + 0 args)))
    (env-bind-builtin '- 1 #t
      (lambda (args)
        (expect-number-list args)
        (cond
          ((null? (cdr args)) (- (car args)))
          (else (fold-left - (car args) (cdr args))))))
    (env-bind-builtin '= 2 #t
      (lambda (args)
        (expect-number-list args)
        (let loop ((args args))
          (define kar (car args))
          (define kdr (cdr args))
          (cond
            ((null? kdr) #t)
            ((= kar (car kdr)) (loop kdr))
            (else #f)))))
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
    ((tagged? prim-lambda-tag prim)
      (let*
        ( (lam (untag prim))
          (arg (prim-lambda.arg lam))
          (mid-prims (prim-lambda.mid-prims lam))
          (last-prim (prim-lambda.last-prim lam)))
        (v-lambda-tagged env arg mid-prims last-prim)))
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
        (define (raise-argn-error required got)
          (raise-v-error
            "wrong number of arguments: required "
            required ", but got " argn "\n"
            (msg-w expr)))
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
                (raise-argn-error argn-min argn))))
          ; lambda
          ((tagged? v-lambda-tag callee-v)
            (let*
              ( (lam (untag callee-v))
                (mid-prims (v-lambda.mid-prims lam))
                (last-prim (v-lambda.last-prim lam))
                (lam-env
                  (let bind
                    ( (lam-env (v-lambda.env lam))
                      (lam-arg (v-lambda.arg lam))
                      (args-v args-v))
                    (cond
                      ((symbol? lam-arg)
                        (env-extend
                          lam-arg
                          (fold-right v-pair-tagged '() args-v)
                          lam-env))
                      ((and (null? lam-arg) (null? args-v)) lam-env)
                      ((and (pair? lam-arg) (pair? args-v))
                        (bind
                          (env-extend (car lam-arg) (car args-v) lam-env)
                          (cdr lam-arg)
                          (cdr args-v)))
                      (else (raise-argn-error (v-lambda.argn-min lam) argn))))) )
              (for-each
                (lambda (prim) (eval-prim prim top-env lam-env))
                mid-prims)
              (eval-prim last-prim top-env lam-env)))
          ; procedure ではない
          (else (raise-v-error "invalid application: " (msg-w expr))))))
    (else (error "eval-prim: fatal error:" prim))))


; returns (value top-env)
(define (eval-toplevel toplevel top-env)
  (cond
    ((match? '('define . _) toplevel)
      (let*
        ( (def-parsed (parse-define toplevel))
          (var (car def-parsed))
          (prim (cadr def-parsed))
          (value (eval-prim prim env-empty top-env)))
        (list v-command-ret (env-define var value top-env))))
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
