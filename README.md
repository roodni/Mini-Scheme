# Mini-Scheme

## REPL

```
$ gosh -l ./mini.scm
gosh> (mini-repl)
```
or
```
$ gosh ./repl.scm
```

## TEST
```
$ gosh ./test.scm
```

```
$ gosh ./print.scm
```

## LANGUAGE
### TOPLEVEL

```
define load
```

### SPECIAL FORMS
```
lambda quote set! let let* letrec if cond and or begin do guard
```

### PROCEDURES
```
number? real? + - * / = < <= > >=
```

```
null? pair? list?
car cdr cons list set-car! set-cdr!
cadr cddr caddr cdddr cadddr
fold fold-left fold-right map for-each
reverse length last append memq assq
```

```
symbol?
```

```
string? string-append
symbol->string string->symbol string->number number->string
```

```
boolean? not
```

```
procedure?
```

```
eq? eqv? equal?
```

```
input-port? eof-object?
display write newline flush
open-input-file close-port read
```

```
raise error
<system-error> <read-error> <unhandled-signal-error>
```