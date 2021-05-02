(load "./check-error.scm")

(check-error (quote (

(cons 1)

)))

(check-error (quote (

(cons 1 2 3)

)))