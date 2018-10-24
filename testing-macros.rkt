#lang racket


#| Use this to document a function with simple
   unit tests |#
(define-syntax-rule
  (blueprint f [in ... out] ...)
  (module+ test
    (require rackunit)
    (check-equal? (f in ...) out) ...))

#| Every time a function defined with this form is
   run, it outputs a trace to the interactions including
   its input arguments and final result |#
(define-syntax-rule
  (define-trace (<name <args ...) <body ...)
  (define (<name <args ...)
    (define result (begin <body ...))
    (println `(<name (<args : ,<args) ... returning: ,result))
    result))

#| Every time a function defined with this form is
   run, it outputs code for a test corresponding
   to that run to the interactions |#
(define-syntax-rule
  (define-maketest (<name <args ...) <body ...)
  (define (<name <args ...)
    (define result (begin <body ...))
    (println `(module+ test
                (require rackunit)
                (check-equal? (<name ',<args ...) ,result)))
    result))