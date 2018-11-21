#lang racket

#|

experimenting with building up a language from minimal provides


|#

(provide #;(except-out (all-from-out racket)
                     #%top #%app)
         #%module-begin ; otherwise, immediate error
         #%top-interaction ; to activate interactions
         ; we seem to have identifiers for free
         #%datum ; can't enter anything
         ; (seems to give ALL atomic datums though, how can i restrict this?)
         ; if we include quote, we have list/symbol literals without it
         app #;#%app
         λ
         #; null ; needed to enter the empty list. or instead we could use quote
         #;quote ; allow list literals
         #;(rename-out [top #%top] [app #%app]))

(define-syntax-rule (app xs ...)
  (#%app xs ...))


#;(define-syntax-rule (top . x) 'x)
