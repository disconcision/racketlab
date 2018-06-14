#lang racket

(require "racket-match-lab.rkt")


; just make a simple tranformation generation system
; for a fixed simple lang ie lc or plastic;

; try to use my runtime pattern matching?

; then gereralize to generating basic creators/destructors from grammar

; think about but dont prioritize:
; autocomplete

(ratch #hash() '(1 2 3)
  ((a b c)
   (c b a)))

; a grammar

#;(grammar
   #; (id (TERM symbol?))
   #; (d (TERM literal?))
   #; (expr (var id)
            (dat d)
            (pat → expr)
            #;(λ (pat → expr) ...)
            (app expr ...)
            (expr ...))
   #; (pat (pvar id)
           (dat d)
           (pat ...)))

; simpler grammar
#;(grammar
   (s (terminal (curry equal? 'expr)))
   (expr (⊙ s)
         (pair expr expr)))

#;(define sorts
    #hash((expr . ((dat d)
                   (app expr expr)))))
#;(define constructors
    #hash((dat . (→ expr))
          (app . (expr expr → expr))))
; (signature approach)
; alternatively, use first-class patterns
; this makes is just the inverse hash from sorts
; provided names are interpreted appropriately

#;(define (parse-grammar grammar)
    (λ (x)
      (runtime-match x)))

; ? write something to turn a hash
; with first-class patterns as keys
; into a fallthrough-matcher... but order...

; idea: flip a switch to change which is implied, list or app

; top ◇
; cursor ▹
; hole ⊙
; hole-ellipses ⊙...
#; ((_... (▹ whatever) ⊙...) → (_... whatever (▹ (⊙ pat)) ⊙...))
; BUT
#; ((_... (▹ (⊙ pat)) ⊙... (▹ whatever)) → (_... ⊙... (▹ whatever)))
; actually, alternatively to above:
; select ⊙... normally, but on transformation, clone it to the right
; another alternative: when ⊙... selected, ENTER expands it hole + ⊙...

; start state
#; (◇ (▹ (⊙ expr)))

#;(define-syntax-rule (ratch whatever
                        x ...
                        (y ⋱→ z)
                        w ...)
    (ratch whatever
      x ...
      ((a ⋱ y) (a ⋱ z))
      w ...))


(define literals
  #hash((dat . ())
        (void . ())
        (box . ())
        (pair . ())
        (◇ . ())
        (▹ . ())
        (⊙ . ())
        (expr . ())))
(let loop
  ([stx '(◇ (▹ (⊙ expr)))])
  (displayln stx)
  (define transform
    (match (read)
      ['0 '([(ctx ⋱ (▹ (⊙ expr)))
             (ctx ⋱ (pair (▹ (⊙ expr)) (⊙ expr)))])]
      ['u '([(◇ a ... (▹ b) c ...)
             (◇ a ... (▹ b) c ...)]  ; add runtime-match macro for guards like this
            [(ctx ⋱ (a ... (▹ b) c ...))
             (ctx ⋱ (▹ (a ... b c ...)))])]
      ['d '([(ctx ⋱ (▹ (⊙ _)))
             (ctx ⋱ (▹ (⊙ _)))]
            [(ctx ⋱ (▹ (pair a b)))
             (ctx ⋱ (pair (▹ a) b))])]
      ['l '([(ctx ⋱ (pair (▹ c) d ...))
             (ctx ⋱ (pair (▹ c) d ...))]
            [(ctx ⋱ (a ... b (▹ c) d ...))
             (ctx ⋱ (a ... (▹ b) c d ...))])]
      ['r '([(ctx ⋱ (a ... (▹ b) c d ...))
             (ctx ⋱ (a ... b (▹ c) d ...))])]
      ['x '([(ctx ⋱ (▹ (pair a b)))
             (ctx ⋱ (▹  (⊙ expr)))])]
      [_ (displayln "that's not a thing")
         '([_ _])]))
  (loop (match (runtime-match literals transform stx)
          ['no-match stx]
          [res res])))

;fn takes
;input = sort of syntax at cursor
;output = possible transformations to fill it
; ⊙ is (hole <type>)
#; ((⊙ pat)
    (list (⊙ pat) → (pvar (⊙ id))
          (⊙ pat) → (dat (⊙ d))
          (⊙ pat) → ((⊙ pat) ⊙...)))


