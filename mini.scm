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
          (pat-end (caddr pat)) )
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
          (pat-r (caddr pat)) )
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
;; const
(define prim-const-tag 'prim-const)
;; call
(define prim-call-tag 'prim-call)
(define (prim-call-tagged callee args expr)
  (assert (list? args))
  (tag prim-call-tag (list callee args expr)))
(define (prim-call.callee prim) (car prim))
(define (prim-call.args prim) (cadr prim))
(define (prim-call.expr prim) (caddr prim))
;; begin
(define prim-begin-tag 'prim-begin)
(define (prim-begin-tagged mid-prims last-prim)
  (assert (list? mid-prims))
  (tag prim-begin-tag (list mid-prims last-prim)))
(define (prim-begin.mid-prims b) (car b))
(define (prim-begin.last-prim b) (cadr b))
;; lambda
(define prim-lambda-tag 'prim-lambda)
(define (prim-lambda-tagged arg body)
  (tag prim-lambda-tag (list arg body)))
(define (prim-lambda.arg prim) (car prim))
(define (prim-lambda.body prim) (cadr prim))
;; if
(define prim-if-tag 'prim-if)
(define (prim-if-tagged condition th el)
  (tag prim-if-tag (list condition th el)))
(define (prim-if.cond i) (car i))
(define (prim-if.then i) (cadr i))
(define (prim-if.else i) (caddr i))
;; and
(define prim-and-tag 'prim-and)
(define (prim-and-tagged l r)
  (tag prim-and-tag (list l r)))
(define (prim-and.l a) (car a))
(define (prim-and.r a) (cadr a))
;; or
(define prim-or-tag 'prim-or)
(define (prim-or-tagged l r)
  (tag prim-or-tag (list l r)))
(define (prim-or.l a) (car a))
(define (prim-or.r a) (cadr a))
;; set!
(define prim-set!-tag 'prim-set!)
(define (prim-set!-tagged var prim)
  (assert (symbol? var))
  (tag prim-set!-tag (list var prim)))
(define (prim-set!.var set) (car set))
(define (prim-set!.prim set) (cadr set))
;; guard
(define prim-guard-tag 'prim-guard)
(define (prim-guard-tagged var cond-clauses body)
  (assert (symbol? var))
  (assert (= (length cond-clauses) 2))
  (assert (list? (car cond-clauses)))
  (tag prim-guard-tag (list var cond-clauses body)))
(define (prim-guard.var g) (car g))
(define (prim-guard.test-clauses g) (car (cadr g)))
(define (prim-guard.else-prim-opt g) (cadr (cadr g)))
(define (prim-guard.body g) (caddr g))

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
          (body (parse-body (cddr def) def)) )
        (list var (prim-lambda-tagged arg body))))
    (else (raise-parse-error def))))

(define (parse-body body error-expr)
  (let loop-define
    ( (defs (list))
      (body body) )
    (cond
      ((match? '(('define . _) . _) body)
        (loop-define
          (cons (parse-define (car body)) defs)
          (cdr body)))
      (else
        (let*
          ( (parsed-exprs (parse-expr+ body error-expr))
            (mid-prims (car parsed-exprs))
            (last-prim (cadr parsed-exprs)) )
          (cond
            ((null? defs)
              (prim-begin-tagged mid-prims last-prim))
            (else
              (let*
                ( (defs (reverse defs))
                  (def-vars (map car defs))
                  (def-set!-list
                    (map
                      (lambda (def)
                        (prim-set!-tagged (car def) (cadr def)))
                      defs))
                  (def-voids (map (lambda (_) (tag prim-const-tag v-void)) defs)) )
                (prim-call-tagged
                  (prim-lambda-tagged
                    def-vars
                    (prim-begin-tagged
                      (append def-set!-list mid-prims)
                      last-prim))
                  def-voids
                  error-expr)))))))))

; returns (mid-prims last-prim)
(define (parse-expr+ exprs error-expr)
  (let parse-expr+
    ( (mid-prims-rev (list))
      (exprs exprs) )
    (cond
      ((match? '(_) exprs)
        (let*
          ( (last-expr (car exprs))
            (last-prim (parse-expr last-expr))
            (mid-prims (reverse mid-prims-rev)) )
          (list mid-prims last-prim)))
      ((match? '(_ . _) exprs)
        (let*
          ( (mid-expr (car exprs))
            (mid-prim (parse-expr mid-expr)) )
          (parse-expr+
            (cons mid-prim mid-prims-rev)
            (cdr exprs))))
      (else (raise-parse-error error-expr)))))

; returns (var prim)*
(define (parse-bindings bindings error-expr)
  (cond
    ((match? '(* (symbol _) ()) bindings)
      (map
        (lambda (b)
          (list
            (car b)
            (parse-expr (cadr b))))
        bindings))
    (else (raise-parse-error error-expr))))

; returns ((test-prim then-prim)* else-prim-opt)
(define (parse-cond-clauses clauses error-expr)
  (let loop
    ( (parsed-clauses-rev '())
      (clauses clauses) )
    (cond
      ((null? clauses)
        (list
          (reverse parsed-clauses-rev)
          #f))
      ((match? '(('else . _)) clauses)
        (let*
          ( (clause (car clauses))
            (prim+ (parse-expr+ (cdr clause) error-expr)) )
          (list
            (reverse parsed-clauses-rev)
            (prim-begin-tagged (car prim+) (cadr prim+)))))
      ((match? '(('else . _) _ . _) clauses)
        (raise-parse-error error-expr))
      ((match? '((_ . _) . _) clauses)
        (let*
          ( (clause (car clauses))
            (test-prim (parse-expr (car clause)))
            (prim+ (parse-expr+ (cdr clause) error-expr))
            (parsed-clause
              (list
                test-prim
                (prim-begin-tagged (car prim+) (cadr prim+)))) )
          (loop
            (cons parsed-clause parsed-clauses-rev)
            (cdr clauses))))
      (else (raise-parse-error error-expr)))))

(define (parse-expr expr)
  (cond
    ((symbol? expr) expr)
    ((number? expr) (tag prim-const-tag expr))
    ((boolean? expr) (tag prim-const-tag expr))
    ((string? expr) (tag prim-const-tag expr))
    ((null? expr) (tag prim-const-tag expr))
    ((match? '('quote . _) expr)
      (cond
        ((match? '(_) (cdr expr))
          (tag prim-const-tag
            (obj->v
              (cadr expr)
              (lambda () (raise-parse-error expr)))))
        (else (raise-parse-error expr))))
    ((match? '('begin . _) expr)
      (cond
        ((null? (cdr expr)) (tag prim-const-tag v-command-ret))
        (else
          (let*
            ( (prim+ (parse-expr+ (cdr expr) expr))
              (mid-prims (car prim+))
              (last-prim (cadr prim+)) )
            (prim-begin-tagged mid-prims last-prim)))))
    ((match? '('lambda . _) expr)
      (cond
        ((match? '((* symbol (or () symbol)) . _) (cdr expr))
          (let*
            ( (arg (cadr expr))
              (body (parse-body (cddr expr) expr)) )
            (prim-lambda-tagged arg body)))
        (else (raise-parse-error expr))))
    ((match? '('let . _) expr)
      (cond
        ((match? '(symbol _ . _) (cdr expr))  ; named let
          (let*
            ( (name (cadr expr))
              (bindings (parse-bindings (caddr expr) expr))
              (body (parse-body (cdddr expr) expr))
              (bindings-vars (map car bindings))
              (bindings-prims (map cadr bindings)) )
            (prim-call-tagged
              (prim-lambda-tagged
                (list name)
                (prim-begin-tagged
                  (list
                    (prim-set!-tagged name
                      (prim-lambda-tagged bindings-vars body)))
                  (prim-call-tagged name bindings-prims expr)))
              (list (tag prim-const-tag v-void))
              expr)))
        ((match? '(_ . _) (cdr expr)) ; let
          (let*
            ( (bindings (parse-bindings (cadr expr) expr))
              (body (parse-body (cddr expr) expr))
              (bindings-vars (map car bindings))
              (bindings-prims (map cadr bindings)) )
            (prim-call-tagged
              (prim-lambda-tagged bindings-vars body)
              bindings-prims
              expr)))
        (else (raise-parse-error expr))))
    ((match? '('let* . _) expr)
      (cond
        ((match? '(_ . _) (cdr expr))
          (let*
            ( (bindings (parse-bindings (cadr expr) expr))
              (body (parse-body (cddr expr) expr)) )
            (fold-right
              (lambda (binding right)
                (define var (car binding))
                (define prim (cadr binding))
                (prim-call-tagged
                  (prim-lambda-tagged (list var) right)
                  (list prim)
                  expr))
              body
              bindings)))
        (else (raise-parse-error expr))))
    ((match? '('letrec . _) expr)
      (cond
        ((match? '(_ . _) (cdr expr))
          (let*
            ( (bindings (parse-bindings (cadr expr) expr))
              (body (parse-body (cddr expr) expr))
              (bindings-vars (map car bindings))
              (bindings-set!-list
                (map
                  (lambda (binding)
                    (prim-set!-tagged (car binding) (cadr binding)))
                  bindings))
              (bindings-voids (map (lambda (_) (tag prim-const-tag v-void)) bindings)) )
            (prim-call-tagged
              (prim-lambda-tagged
                bindings-vars
                (prim-begin-tagged
                  bindings-set!-list
                  body))
              bindings-voids
              expr)))
        (else (raise-parse-error expr))))
    ((match? '('if . _) expr)
      (cond
        ((match? '(_ _ . (or (_) ())) (cdr expr))
          (let*
            ( (condition (cadr expr))
              (th (caddr expr))
              (el-opt (cdddr expr)) )
            (prim-if-tagged
              (parse-expr condition)
              (parse-expr th)
              (if (null? el-opt)
                (tag prim-const-tag v-command-ret)
                (parse-expr (car el-opt))))))
        (else (raise-parse-error expr))))
    ((match? '('and . _) expr)
      (cond
        ((null? (cdr expr)) (tag prim-const-tag #t))
        (else
          (let*
            ( (prim+ (parse-expr+ (cdr expr) expr))
              (mid-prims (car prim+))
              (last-prim (cadr prim+)) )
            (fold-right prim-and-tagged last-prim mid-prims)))))
    ((match? '('or . _) expr)
      (cond
        ((null? (cdr expr)) (tag prim-const-tag #f))
        (else
          (let*
            ( (prim+ (parse-expr+ (cdr expr) expr))
              (mid-prims (car prim+))
              (last-prim (cadr prim+)) )
            (fold-right prim-or-tagged last-prim mid-prims)))))
    ((match? '('cond . _) expr)
      (let*
        ( (clauses (parse-cond-clauses (cdr expr) expr))
          (test-clauses (car clauses))
          (else-prim
            (or (cadr clauses)
                (tag prim-const-tag v-command-ret))) )
        (fold-right
          (lambda (clause else-prim)
            (define test-prim (car clause))
            (define then-prim (cadr clause))
            (prim-if-tagged test-prim then-prim else-prim))
          else-prim
          test-clauses)))
    ((match? '('set! . _) expr)
      (cond
        ((match? '(symbol _) (cdr expr))
          (prim-set!-tagged
            (cadr expr)
            (parse-expr (caddr expr))))
        (else (raise-parse-error expr))))
    ((match? '('guard . _) expr)
      (cond
        ((match? '((symbol . _) . _) (cdr expr))
          (let*
            ( (var (car (cadr expr)))
              (clauses (cdr (cadr expr)))
              (clauses (parse-cond-clauses clauses expr))
              (body (parse-body (cddr expr) expr)) )
            (prim-guard-tagged var clauses body)))
        (else (raise-parse-error expr))))
    ((list? expr)
      (let
        ( (prims
            (map
              (lambda (e) (parse-expr e))
              expr)) )
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
(define (v-lambda-tagged env arg body)
  (tag v-lambda-tag (list env arg body)))
(define (v-lambda.env lam) (car lam))
(define (v-lambda.arg lam) (cadr lam))
(define (v-lambda.argn-min lam)
  (let count ((arg (v-lambda.arg lam)) (n 0))
    (cond
      ((pair? arg) (count (cdr arg) (+ n 1)))
      (else n))))
(define (v-lambda.body lam) (caddr lam))
;; pair
(define v-pair-tag 'v-pair)
(define (v-pair-tagged kar kdr)
  (tag v-pair-tag (cons kar kdr)))
(define (v-pair.car p) (car p))
(define (v-pair.cdr p) (cdr p))
(define (v-pair.set-car! p v) (set-car! p v))
(define (v-pair.set-cdr! p v) (set-cdr! p v))
;; error
(define v-error-tag 'v-error)
(define (raise-v-error . l)
  (raise (tag v-error-tag (msg l))))
;; void (変数から取り出すとエラー)
(define v-void-tag 'v-void)
(define v-void (tag v-void-tag '()))
;; 命令の戻り値
(define v-command-ret '())

(define (v-print base-print v)
  (let v-print
    ( (is-cdr #f)
      (pair-memo '())
      (v v) )
    (cond
      ((not (mini-value? v))
        (display "[FATAL: ")
        (write v)
        (display "]"))
      ((symbol? v) (base-print v))
      ((number? v) (base-print v))
      ((boolean? v) (base-print v))
      ((string? v) (base-print v))
      ((null? v) (base-print v))
      ((tagged? v-builtin-tag v)
        (let*
          ( (v (untag v))
            (name (v-builtin.name v)) )
          (display "[built-in ")
          (display name)
          (display "]")))
      ((tagged? v-lambda-tag v)
        (let*
          ( (lam (untag v))
            (arg (v-lambda.arg lam)) )
          (display "[lambda ")
          (write arg)
          (display " ...]")))
      ((tagged? v-pair-tag v)
        (cond
          ((memq v pair-memo)
            (if is-cdr (display ". "))
            (display "[circular]"))
          (else
            (let*
              ( (pair-memo (cons v pair-memo))
                (v (untag v))
                (a (v-pair.car v))
                (d (v-pair.cdr v)) )
              (if (not is-cdr) (display "("))
              (v-print #f pair-memo a)
              (cond
                ((null? d) 'do-nothing)
                ((tagged? v-pair-tag d)
                  (display " ")
                  (v-print #t pair-memo d))
                (else
                  (display " . ")
                  (v-print #f pair-memo d)))
              (if (not is-cdr) (display ")"))))))
      ((tagged? v-error-tag v)
        (display "[error: ")
        (msg-print (untag v))
        (display "]"))
      ((eof-object? v) (display "[eof]"))
      ((input-port? v) (display "[input-port]"))
      (else
        (display "[")
        (write v)
        (display "]")))))

(define (v-display v) (v-print display v))
(define (v-write v) (v-print write v))

(define (mini-value? obj)
  (if
    (or
      (symbol? obj)
      (number? obj)
      (boolean? obj)
      (string? obj)
      (null? obj)
      (tagged? v-builtin-tag obj)
      (tagged? v-lambda-tag obj)
      (tagged? v-pair-tag obj)
      (tagged? v-error-tag obj)
      (tagged? v-void-tag obj)
      (eof-object? obj)
      (input-port? obj)
      (<read-error> obj)
      (<system-error> obj))
    #t #f))

(define (v->obj v)
  (cond
    ((symbol? v) v)
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

(define (obj->v obj raiser)
  (let obj->v ((obj obj))
    (cond
      ((symbol? obj) obj)
      ((number? obj) obj)
      ((boolean? obj) obj)
      ((string? obj) obj)
      ((null? obj) obj)
      ((pair? obj)
        (v-pair-tagged
          (obj->v (car obj))
          (obj->v (cdr obj))))
      (else (raiser)))))

;;; env
(define (env-bind var value) (cons var value))
(define (env-bind.value bind) (cdr bind))
(define (env-bind-set! bind value) (set-cdr! bind value))

(define env-empty '())
(define (env-lookup var env)
  (assq var env))
(define (env-extend var value env)
  (cons (env-bind var value) env))
(define (env-define var value env)
  (let ((bind (env-lookup var env)))
    (cond
      (bind
        (env-bind-set! bind value)
        env)
      (else (env-extend var value env)))))

(define builtin-error-tag 'builtin-error)
(define (raise-builtin-error . l)
  (raise (tag builtin-error-tag (msg l))))

(define (env-init)
  (define (env-bind-builtin var argn-min variadic proc)
    (env-bind
      var
      (v-builtin-tagged
        (symbol->string var)
        argn-min variadic proc)))
  (define (raise-type-error ty v)
    (raise-builtin-error ty " required, but got " (msg-v v)))
  (define (expect-number-list vl)
    (for-each
      (lambda (x)
        (if (not (number? x)) (raise-type-error "number" x)))
      vl))
  (define (expect-real-list vl)
    (for-each
      (lambda (x)
        (if (not (real? x)) (raise-type-error "real number" x)))
      vl))
  (define (transitive-relation-hold? rel lis)
    (define kar (car lis))
    (define kdr (cdr lis))
    (cond
      ((null? kdr) #t)
      ((rel kar (car kdr))
        (transitive-relation-hold? rel kdr))
      (else #f)))
  (define env
    (list
      (env-bind-builtin 'cons 2 #f
        (lambda (args)
          (define a (car args))
          (define d (cadr args))
          (v-pair-tagged a d)))
      (env-bind-builtin 'list 0 #t
        (lambda (args) (fold-right v-pair-tagged '() args)))
      (env-bind-builtin 'car 1 #f
        (lambda (args)
          (define p (car args))
          (cond
            ((tagged? v-pair-tag p) (v-pair.car (untag p)))
            (else (raise-type-error "pair" p)))))
      (env-bind-builtin 'cdr 1 #f
        (lambda (args)
          (define p (car args))
          (cond
            ((tagged? v-pair-tag p) (v-pair.cdr (untag p)))
            (else (raise-type-error "pair" p)))))
      (env-bind-builtin 'set-car! 2 #f
        (lambda (args)
          (define p (car args))
          (define v (cadr args))
          (cond
            ((tagged? v-pair-tag p)
              (v-pair.set-car! (untag p) v)
              v-command-ret)
            (else (raise-type-error "pair" p)))))
      (env-bind-builtin 'set-cdr! 2 #f
        (lambda (args)
          (define p (car args))
          (define v (cadr args))
          (cond
            ((tagged? v-pair-tag p)
              (v-pair.set-cdr! (untag p) v)
              v-command-ret)
            (else (raise-type-error "pair" p)))))
      (env-bind-builtin 'null? 1 #f
        (lambda (args) (null? (car args))))
      (env-bind-builtin 'pair? 1 #f
        (lambda (args) (tagged? v-pair-tag (car args))))
      (env-bind-builtin 'symbol? 1 #f
        (lambda (args) (symbol? (car args))))
      (env-bind-builtin 'string? 1 #f
        (lambda (args) (string? (car args))))
      (env-bind-builtin 'number? 1 #f
        (lambda (args) (number? (car args))))
      (env-bind-builtin 'real? 1 #f
        (lambda (args) (real? (car args))))
      (env-bind-builtin 'boolean? 1 #f
        (lambda (args) (boolean? (car args))))
      (env-bind-builtin 'procedure? 1 #f
        (lambda (args)
          (define obj (car args))
          (or (tagged? v-lambda-tag obj)
              (tagged? v-builtin-tag obj))))
      (env-bind-builtin 'input-port? 1 #f
        (lambda (args) (input-port? (car args))))
      (env-bind-builtin 'eof-object? 1 #f
        (lambda (args) (eof-object? (car args))))
      (env-bind-builtin 'eq? 2 #f
        (lambda (args) (eq? (car args) (cadr args))))
      (env-bind-builtin 'equal? 2 #f
        (lambda (args)
          (let loop ((l (car args)) (r (cadr args)))
            (define (test q?) (and (q? l) (q? r)))
            (cond
              ((test (lambda (x) (tagged? v-pair-tag x)))
                (let ((l (untag l)) (r (untag r)))
                  (and
                    (loop (v-pair.car l) (v-pair.car r))
                    (loop (v-pair.cdr l) (v-pair.cdr r)))))
              ((test number?) (equal? l r))
              ((test string?) (equal? l r))
              (else (eq? l r))))))
      (env-bind-builtin '+ 0 #t
        (lambda (args)
          (expect-number-list args)
          (fold + 0 args)))
      (env-bind-builtin '* 0 #t
        (lambda (args)
          (expect-number-list args)
          (fold * 1 args)))
      (env-bind-builtin '- 1 #t
        (lambda (args)
          (expect-number-list args)
          (cond
            ((null? (cdr args)) (- (car args)))
            (else (fold-left - (car args) (cdr args))))))
      (env-bind-builtin '/ 1 #t
        (lambda (args)
          (define (safe/ x y)
            (if (= y 0) (raise-builtin-error "zero division")
              (/ x y)))
          (expect-number-list args)
          (cond
            ((null? (cdr args)) (safe/ 1 (car args)))
            (else (fold-left safe/ (car args) (cdr args))))))
      (env-bind-builtin '= 2 #t
        (lambda (args)
          (expect-number-list args)
          (transitive-relation-hold? = args)))
      (env-bind-builtin '< 2 #t
        (lambda (args)
          (expect-real-list args)
          (transitive-relation-hold? < args)))
      (env-bind-builtin '> 2 #t
        (lambda (args)
          (expect-real-list args)
          (transitive-relation-hold? > args)))
      (env-bind-builtin '>= 2 #t
        (lambda (args)
          (expect-real-list args)
          (transitive-relation-hold? >= args)))
      (env-bind-builtin '<= 2 #t
        (lambda (args)
          (expect-real-list args)
          (transitive-relation-hold? <= args)))
      (env-bind-builtin 'display 1 #f
        (lambda (args)
          (v-display (car args))
          v-command-ret))
      (env-bind-builtin 'write 1 #f
        (lambda (args)
          (v-write (car args))
          v-command-ret))
      (env-bind-builtin 'newline 0 #f
        (lambda (_) (newline) v-command-ret))
      (env-bind-builtin 'flush 0 #f
        (lambda (args)
          (flush)
          v-command-ret))
      (env-bind-builtin 'symbol->string 1 #f
        (lambda (args)
          (define sym (car args))
          (cond
            ((symbol? sym) (symbol->string sym))
            (else (raise-type-error "symbol" sym)))))
      (env-bind-builtin 'raise 1 #f
        (lambda (args) (raise (car args))))
      (env-bind-builtin '<system-error> 1 #f
        (lambda (args) (<system-error> (car args))))
      (env-bind-builtin '<read-error> 1 #f
        (lambda (args) (<read-error> (car args))))
      (env-bind-builtin 'read 0 #t
        (lambda (args)
          (let*
            ( (obj
                (cond
                  ((null? args) (read))
                  ((null? (cdr args))
                    (let ((port (car args)))
                      (cond
                        ((input-port? port) (read port))
                        (else (raise-type-error "input-port" port)))))
                  (else (raise-builtin-error "too many arguments")))) )
            (if (eof-object? obj) obj
              (obj->v
                obj
                (lambda ()
                  (raise-builtin-error "read: unimplemented expression")))))))
      (env-bind-builtin 'open-input-file 1 #f
        (lambda (args)
          (define filename (car args))
          (cond
            ((string? filename) (open-input-file filename))
            (else (raise-type-error "string" filename)))))
      (env-bind-builtin 'close-port 1 #f
        (lambda (args)
          (define port (car args))
          (cond
            ((input-port? port) (close-port port))
            (else (raise-type-error "port" port)))
          v-command-ret))
    ))
  (define program '(
    (define (not obj) (if obj #f #t))

    (define (cadr obj) (car (cdr obj)))
    (define (cddr obj) (cdr (cdr obj)))
    (define (caddr obj) (car (cddr obj)))
    (define (cdddr obj) (cdr (cddr obj)))
    (define (cadddr obj) (car (cdddr obj)))

    (define (fold kons knil lis)
      (let fold ((knil knil) (lis lis))
        (if (null? lis) knil
          (fold (kons (car lis) knil) (cdr lis)))))
    (define (reverse lis) (fold cons '() lis))
    (define (fold-left snok knil lis)
      (define (kons a b) (snok b a))
      (fold kons knil lis))

    (define (fold-right kons knil lis)
      (let fold-right ((lis lis))
        (if (null? lis) knil
          (kons
            (car lis)
            (fold-right (cdr lis))))))
    (define (map proc lis)
      (fold-right
        (lambda (x rest)
          (cons (proc x) rest))
        '() lis))
    
    (define (for-each proc lis)
      (let for-each ((lis lis))
        (cond
          ((null? lis) '())
          (else
            (proc (car lis))
            (for-each (cdr lis))))))

    (define (length lis)
      (let loop ((n 0) (lis lis))
        (if (null? lis) n (loop (+ n 1) (cdr lis)))))
    
    (define (memq obj lis)
      (let memq ((lis lis))
        (cond
          ((null? lis) #f)
          ((eq? obj (car lis)) lis)
          (else (memq (cdr lis))))))
    (define (assq obj alist)
      (let assq ((alist alist))
        (cond
          ((null? alist) #f)
          ((eq? obj (car (car alist))) (car alist))
          (else (assq (cdr alist))))))

    (define (append . lists)
      (let loop ((mid-lists-rev '()) (lists lists))
        (cond
          ((null? lists) '())
          ((null? (cdr lists))
            (fold
              (lambda (lis appended)
                (fold-right cons appended lis))
              (car lists)
              mid-lists-rev))
          (else
            (loop
              (cons (car lists) mid-lists-rev)
              (cdr lists))))))
    
    (define (list? obj)
      (let list? ((mem '()) (obj obj))
        (cond
          ((null? obj) #t)
          ((pair? obj)
            (if (memq obj mem) #f
              (list? (cons obj mem) (cdr obj))))
          (else #f))))
  ))
  (cadr (eval-toplevel-list program env)))



;;; eval
(define (eval-prim prim top-env env)
  (cond
    ((symbol? prim)
      (let*
        ( (value
            (env-bind.value
              (or (env-lookup prim env)
                  (env-lookup prim top-env)
                  (raise-v-error "unknown location: " (msg-w prim))))) )
        (if (tagged? v-void-tag value)
          (raise-v-error "uninitialized variable: " (msg-w prim))
          value)))
    ((tagged? prim-set!-tag prim)
      (let*
        ( (set (untag prim))
          (var (prim-set!.var set))
          (bind
            (or (env-lookup var env)
                (env-lookup var top-env)
                (raise-v-error "unknown location: " (msg-w var))))
          (prim (prim-set!.prim set))
          (value (eval-prim prim top-env env)) )
        (env-bind-set! bind value)
        v-command-ret))
    ((tagged? prim-const-tag prim) (untag prim))
    ((tagged? prim-lambda-tag prim)
      (let*
        ( (lam (untag prim))
          (arg (prim-lambda.arg lam))
          (body (prim-lambda.body lam)) )
        (v-lambda-tagged env arg body)))
    ((tagged? prim-if-tag prim)
      (let*  ((i (untag prim)))
        (if (eval-prim (prim-if.cond i) top-env env)
          (eval-prim (prim-if.then i) top-env env)
          (eval-prim (prim-if.else i) top-env env))))
    ((tagged? prim-and-tag prim)
      (let* ((a (untag prim)))
        (and
          (eval-prim (prim-and.l a) top-env env)
          (eval-prim (prim-and.r a) top-env env))))
    ((tagged? prim-or-tag prim)
      (let* ((o (untag prim)))
        (or
          (eval-prim (prim-or.l o) top-env env)
          (eval-prim (prim-or.r o) top-env env))))
    ((tagged? prim-begin-tag prim)
      (let*
        ( (beg (untag prim))
          (mid-prims (prim-begin.mid-prims beg))
          (last-prim (prim-begin.last-prim beg)) )
        (for-each
          (lambda (prim) (eval-prim prim top-env env))
          mid-prims)
        (eval-prim last-prim top-env env)))
    ((tagged? prim-guard-tag prim)
      (let*
        ( (g (untag prim))
          (var (prim-guard.var g))
          (test-clauses (prim-guard.test-clauses g))
          (else-prim-opt (prim-guard.else-prim-opt g))
          (body (prim-guard.body g)) )
        (guard
          (condition
            ((mini-value? condition)
              (let ((env (env-extend var condition env)))
                (let loop ((test-clauses test-clauses))
                  (cond
                    ((null? test-clauses)
                      (if else-prim-opt
                        (eval-prim else-prim-opt top-env env)
                        (raise condition)))
                    (else
                      (let*
                        ( (clause (car test-clauses))
                          (test-prim (car clause))
                          (then-prim (cadr clause)) )
                        (if (eval-prim test-prim top-env env)
                          (eval-prim then-prim top-env env)
                          (loop (cdr test-clauses))))))))))
          (eval-prim body top-env env))))
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
              args-prim)) )
        (define (raise-argn-error required got)
          (raise-v-error
            "wrong number of arguments: required "
            required ", but got " argn "\n  "
            (msg-w expr)))
        (cond
          ; built-in procedure
          ((tagged? v-builtin-tag callee-v)
            (let*
              ( (builtin (untag callee-v))
                (argn-min (v-builtin.argn-min builtin))
                (variadic (v-builtin.variadic? builtin))
                (proc (v-builtin.proc builtin)) )
              (if
                (or
                  (= argn-min argn)
                  (and variadic (< argn-min argn)))
                (guard
                  (err
                    ((tagged? builtin-error-tag err)
                      (raise-v-error (untag err) "\n  " (msg-w expr))))
                  (proc args-v))
                (raise-argn-error argn-min argn))))
          ; lambda
          ((tagged? v-lambda-tag callee-v)
            (let*
              ( (lam (untag callee-v))
                (body (v-lambda.body lam))
                (lam-env
                  (let bind
                    ( (lam-env (v-lambda.env lam))
                      (lam-arg (v-lambda.arg lam))
                      (args-v args-v) )
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
              (eval-prim body top-env lam-env)))
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
          (value (eval-prim prim env-empty top-env)) )
        (list v-command-ret (env-define var value top-env))))
    ((match? '('load string) toplevel)
      (let*
        ( (filename (cadr toplevel))
          (port
            (guard
              (err ((<system-error> err)
                  (raise-v-error "cannot open file: " (msg-w filename))))
              (open-input-file filename)))
          (program
            (guard
              (_ (else
                  (close-port port)
                  (raise-v-error "read failed while loading file: " (msg-w filename))))
              (let loop ((program-rev '()))
                (define toplevel (read port))
                (cond
                  ((eof-object? toplevel) (reverse program-rev))
                  (else
                    (loop (cons toplevel program-rev))))))) )
        (eval-toplevel-list program top-env)))
    (else
      (let*
        ( (prim (parse-expr toplevel))
          (value (eval-prim prim top-env env-empty)) )
        (list value top-env)))))

(define (eval-toplevel-list toplevel-list top-env)
  (fold
    (lambda (toplevel res)
      (define top-env (cadr res))
      (eval-toplevel toplevel top-env))
    (list v-command-ret top-env)
    toplevel-list))

;;; test
(define (expect-error toplevel-list)
  (guard
    (err
      ((tagged? v-error-tag err)
        (display "error: ")
        (msg-print (untag err))
        (newline)))
    (eval-toplevel-list toplevel-list (env-init))
    (display "no error occured:\n")
    (write toplevel-list)
    (newline)
    (exit 1)))

(define (expect-parse-error toplevel-list)
  (guard
    (err
      ((tagged? parse-error-tag err)
        (display "syntax error: ")
        (write (untag err))
        (newline)))
    (eval-toplevel-list toplevel-list (env-init))
    (display "no error occured:\n")
    (write toplevel-list)
    (newline)
    (exit 1)))

;;; repl
(define (mini-repl)
  (display "mini-scheme intepreter\n")
  (let loop ((top-env (env-init)))
    (newline)
    (display "mini> ")
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
                (tag 'ok input))))) )
      (cond
        ((eof-object? toplevel) 'bye)
        ((tagged? 'ok toplevel)
          (let*
            ( (toplevel (untag toplevel))
              (res
                (guard
                  (condition
                    ((tagged? parse-error-tag condition)
                      (display "syntax error: ")
                      (write (untag condition))
                      'error)
                    ((tagged? v-error-tag condition)
                      (display "error: ")
                      (msg-print (untag condition))
                      'error)
                    ((mini-value? condition)
                      (display "uncaught exception: ")
                      (v-write condition)))
                  (tag 'ok
                    (eval-toplevel toplevel top-env)))) )
            (cond
              ((tagged? 'ok res)
                (let*
                  ( (res (untag res))
                    (value (car res))
                    (top-env (cadr res)) )
                  (v-write value)
                  (loop top-env)))
              (else (loop top-env)))))
        (else (loop top-env))))))
