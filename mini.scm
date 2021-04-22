(define (mini-eval toplevel)
  toplevel)

(define (mini-repl)
  (begin
    (display "mini-scheme intepreter\n")
    (let loop ()
    (begin
      (display "\n> ")
      (flush)
      (let ((toplevel (read)))
      (if (eof-object? toplevel)
        ()
        (let ((value (mini-eval toplevel)))
        (begin
          (display value)
          (loop)))))))))