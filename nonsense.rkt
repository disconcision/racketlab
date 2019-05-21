#lang racket

#;(define (quote a) 'a)

#;(define (a) (a))



#;(define (quote define)
    'define)

#;(define (define quote)
    'quote)

(define (quote define)
  (local [(define-syntax-rule (define define)
            'define)]
    '(define quote)))

'quote


#;(let quote ([let void]) 'let)