(load "./mini.scm")


; +
(expect-error '(
  (+ "hello")
))
(expect-error '(
  (+ 1 "hello" 3)
))

; *
(expect-error '(
  (* "")
))

; =
(expect-error '(
  (= #t #f)
))

; -
(expect-error '(
  (- 1 #f)
))

; /
(expect-error '(
  (/ "a")
))

; car
(expect-error '(
  (car 1)
))

; cdr
(expect-error '(
  (cdr 1)
))

; set-car!
(expect-error '(
  (set-car! 1 2)
))

; set-cdr!
(expect-error '(
  (set-cdr! 1 2)
))

; <
(expect-error '(
  (< 1 "hoge")
))
(expect-error '(
  (< 1 2+i 3)
))

; >
(expect-error '(
  (> 1 2+i 3)
))

; >=
(expect-error '(
  (>= 1 2+i 3)
))

; <=
(expect-error '(
  (<= 1 2+i 3)
))

; string-append
(expect-error '(
  (string-append "a" 2 "c")
))

; symbol->string
(expect-error '(
  (symbol->string "a")
))
; string->symbol
(expect-error '(
  (string->symbol 'sym)
))
; string->number
(expect-error '(
  (string->number 12)
))
; number->string
(expect-error '(
  (number->string "")
))

; read
(expect-error '(
  (read "port")
))

; open-input-file
(expect-error '(
  (open-input-file 'filename)
))

; close-port
(expect-error '(
  (close-port "port")
))