(load "./check-error.scm")


(check-error (quote (

(+ "hello")

)))

(check-error (quote (

(+ 1 "hello" 3)

)))