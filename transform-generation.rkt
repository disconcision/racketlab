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
   (d (terminal (curry equal? 'void)))
   (expr (box d)
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
  (define new-stx
    (match (read)
      ['0 (ratch literals stx
            [(ctx ⋱ (▹ (⊙ expr)))
             (ctx ⋱ (pair (▹ (⊙ expr)) (⊙ expr)))])]
      ['u (ratch literals stx
            [(◇ a ... (▹ b) c ...)
             (◇ a ... (▹ b) c ...)]
            [(ctx ⋱ (a ... (▹ b) c ...))
             (ctx ⋱ (▹ (a ... b c ...)))]
            )]
      ['x (ratch literals stx
            [(ctx ⋱ (▹ _))
             (ctx ⋱ (▹ (⊙ expr)))])]
      [_ (displayln "that's not a thing")
         stx]
      #;#;#;
      ['0 (match stx
            [(▹ (⊙ expr)) ⋱→ (▹ (dat void))])]
      ['1 (match stx
            [(▹ (⊙ expr)) ⋱→ (box (▹ (⊙ expr)))])]
      ['2 (match stx
            [(▹ (⊙ expr)) ⋱→ (pair (▹ (⊙ expr)) (⊙ expr))])]
      #;#;#;#;#;
      ['x (match stx
            [(▹ (dat void)) ⋱→ (▹ (⊙ expr))]
            [(▹ (box _)) ⋱→ (▹ (⊙ expr))]
            [(box (▹ (⊙ expr))) ⋱→ (▹ (⊙ expr))])]
      ['u (match stx
            [(a ... (▹ b) c ...) ⋱↦ (▹ (a ... b c ...))]
            [_ stx])]
      ['d (match stx
            [(▹ (a b ...)) ⋱↦ ((▹ a) b ...)]
            [_ stx])]
      ['l (match stx
            [(a ... b (▹ c) d ...) ⋱↦ (a ... (▹ b) c d ...)]
            [_ stx])]
      ['r (match stx
            [(a ... b (▹ c) d ...) ⋱↦ (a ... (▹ b) c d ...)]
            [_ stx])]))
  (loop (if (equal? new-stx 'no-match)
            stx
            new-stx)))

;fn takes
;input = sort of syntax at cursor
;output = possible transformations to fill it
; ⊙ is (hole <type>)
#; ((⊙ pat)
    (list (⊙ pat) → (pvar (⊙ id))
          (⊙ pat) → (dat (⊙ d))
          (⊙ pat) → ((⊙ pat) ⊙...)))


