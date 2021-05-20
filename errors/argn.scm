(load "./mini.scm")

; cons
(expect-error '(
  (cons 1)
))

(expect-error '(
  (cons 1 2 3)
))

; =
(expect-error '(
  (=)
))
(expect-error '(
  (= 0)
))

; <
(expect-error '(
  (<)
))
(expect-error '(
  (< 0)
))

; >
(expect-error '(
  (>)
))
(expect-error '(
  (> 0)
))

; -
(expect-error '(
  (-)
))

; /
(expect-error '(
  (/)
))

; lambda
(expect-error '(
  ((lambda (x y . z) x) 1)
))

(expect-error '(
  ((lambda (x) x) 1 2)
))

(expect-error '(
  ((lambda (x) x))
))

; car
(expect-error '(
  (define p (cons 1 2))
  (car p p)
))

(expect-error '(
  (car)
))

; cdr
(expect-error '(
  (define p (cons 1 2))
  (cdr p p)
))

(expect-error '(
  (cdr)
))

; set-car!
(expect-error '(
  (define p (cons 1 2))
  (set-car! p)
))

; set-car!
(expect-error '(
  (define p (cons 1 2))
  (set-car! p p p)
))

; set-cdr!
(expect-error '(
  (define p (cons 1 2))
  (set-cdr! p)
))

; set-cdr!
(expect-error '(
  (define p (cons 1 2))
  (set-cdr! p p p)
))

; null?
(expect-error '( (null?) ))
(expect-error '( (null? '() '()) ))

; pair?
(expect-error '( (pair?) ))
(expect-error '( (pair? 1 2) ))

; string?
(expect-error '( (string?) ))
(expect-error '( (string? 1 2) ))

; number?
(expect-error '( (number?) ))
(expect-error '( (number? 1 2) ))

; real?
(expect-error '( (real?) ))
(expect-error '( (real? 1 2) ))

; boolean?
(expect-error '( (boolean?) ))
(expect-error '( (boolean? 1 2) ))

; procedure?
(expect-error '( (procedure?) ))
(expect-error '( (procedure? 1 2) ))

; symbol?
(expect-error '( (symbol?) ))
(expect-error '( (symbol? 'a 'b) ))

; input-port?
(expect-error '( (input-port?) ))
(expect-error '( (input-port? 1 2) ))

; eof-object?
(expect-error '( (eof-object?) ))
(expect-error '( (eof-object? 1 2) ))

; <system-error>
(expect-error '( (<system-error>) ))
(expect-error '( (<system-error> 1 2) ))

; <read-error>
(expect-error '( (<read-error>) ))
(expect-error '( (<read-error> 1 2) ))

; symbol->string
(expect-error '( (symbol->string) ))
(expect-error '( (symbol->string 'a 'b) ))
; string->symbol
(expect-error '( (string->symbol) ))
(expect-error '( (string->symbol "a" "b") ))
; string->number
(expect-error '( (string->number) ))
(expect-error '( (string->number "a" "b") ))
; number->string
(expect-error '( (number->string) ))
(expect-error '( (number->string 1 2) ))

; eq?
(expect-error '( (eq? 'a) ))
(expect-error '( (eq? 'a 'b 'c) ))

; eqv?
(expect-error '( (eqv? 'a) ))
(expect-error '( (eqv? 'a 'b 'c) ))

; equal?
(expect-error '( (equal? 'a) ))
(expect-error '( (equal? 'a 'b 'c) ))

; read
(expect-error '( (read 1 2) ))

; open-input-file
(expect-error '( (open-input-file) ))
(expect-error '( (open-input-file "a" "b") ))

; close-port
(expect-error '( (close-port) ))
(expect-error '( (close-port 1 2) ))