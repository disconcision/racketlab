#lang racket


(require (for-syntax racket/base syntax/parse syntax/free-vars))
(define-syntax (print-body-free-vars stx)
    (syntax-parse stx
      #:literals (lambda)
      [(_ (~and lam (lambda (a ...) b ...)))
       (define expanded-body (local-expand #'lam 'expression '()))
       (syntax-parse expanded-body
         #:literals (#%plain-lambda)
         [(#%plain-lambda (arg ...) body)
          (displayln (free-vars #'body))
          expanded-body])]))
#;(lambda (x) (print-body-free-vars (lambda (y) x)))


(define-match-expander %
  (λ (stx)
    (syntax-parse stx
      [(%  <internal-pat>)
       (displayln (syntax-local-context))
       (define expanded-body (local-expand #'(match 0 [<internal-pat> 0]) 'expression '()))
       (displayln #'expanded-body)
       #;(syntax-parse expanded-body
         #:literals (#%plain-lambda)
         [(#%plain-lambda (arg ...) body)
          (displayln (free-vars #'body))
          expanded-body])
       #'(list <internal-pat>)
       ])
    #;(syntax-case stx ()
      [(% context-id <internal-pat>)
       #'(context-id <internal-pat>)]
      ))
  (λ (stx)
    (syntax-parse stx
      [(%  internal-template)
       (define expanded-body (local-expand #'internal-template (syntax-local-context) '()))
       (displayln (free-vars #'expanded-body))
       #'( internal-template)])))

(match '(1)
  [(%  a)
   (%  (list a 1))])