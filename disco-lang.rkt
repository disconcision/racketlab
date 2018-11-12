#lang racket

(provide (except-out (all-from-out racket)
                     λ)
         (rename-out [match-lambda λ]
                     [match-lambda** λ*]#;[app #%app]))


#;(define-syntax-rule (my-λ [<pat <expr ...] ...)
  (match-lambda* [<pat <expr ...] ...))

; TODO: check out match/derived for better error reporting

#|

want to override:
λ
lambda (can use match-λ?)
(λ (x) x) versus (λ [x x]) - same char count
variadic lambda? just do (λ . pairs ...) ?


let (can use match-let)
let*
letrec
define
define/fn
for/list
for/fold

|#

#;(module Maybe racket
  (provide (rename-out (maybe-call #%app) (#%app :)))
  (define-syntax maybe-call
    (syntax-rules ()
      [(maybe-call f e) (match f ['fail 'fail] [f′ (match e ['fail 'fail] [e′ (f′ e′)])])]
      [(maybe-call f e0 e ...) (maybe-call (maybe-call f e0) e ...)])))

#;(require 'Maybe)