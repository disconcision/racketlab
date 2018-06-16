#lang racket

(require "../projects/fructerm/fructerm.rkt")

#| 2018.06.14

 just make a simple tranformation generation system
 for a fixed simple lang ie lc or plastic;

 try to use my runtime pattern matching?

 then gereralize to generating basic creators/destructors from grammar

 think about but dont prioritize:
 - autocomplete


|#



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


; a simpler grammar

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


; ⊙ is (hole <type>)
#; ((⊙ pat)
    (list (⊙ pat) → (pvar (⊙ id))
          (⊙ pat) → (dat (⊙ d))
          (⊙ pat) → ((⊙ pat) ⊙...)))

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
(define initial-state
  #hash((stx . (◇ (▹ (⊙ expr))))
        (transforms . ())
        (messages . ("hello"))))


#;(grammar
   (s (terminal (curry equal? 'expr)))
   (expr (⊙ s)
         (pair expr expr)))

; list of literals (generate from grammar)
(define literals
  #hash((dat . ())
        (void . ())
        (box . ())
        (pair . ())
        (◇ . ())
        (▹ . ())
        (⊙ . ())
        (expr . ())))

; map from keys to functions
(define keymap
  '([0 ([⋱→
         (▹ (⊙ expr))
         (pair (▹ (⊙ expr)) (⊙ expr))])]
    [u ([(◇ a ... (▹ b) c ...)
         (◇ a ... (▹ b) c ...)]
        ; add runtime-match macro for guards like this
        [⋱→
         (a ... (▹ b) c ...)
         (▹ (a ... b c ...))])]
    [d ([⋱→
         (▹ (⊙ _))
         (▹ (⊙ _))]
        [⋱→
         (▹ (pair a b))
         (pair (▹ a) b)])]
    [l ([⋱→
         (pair (▹ c) d ...)
         (pair (▹ c) d ...)]
        [⋱→
         (a ... b (▹ c) d ...)
         (a ... (▹ b) c d ...)])]
    [r ([⋱→
         (a ... (▹ b) c d ...)
         (a ... b (▹ c) d ...)])]
    [x ([⋱→
         (▹ (pair a b))
         (▹  (⊙ expr))])]))


(define (get-transform key)
  (let ([res (assoc key keymap)])
    (if res (second res) '([a a]))))

; perform a sequence of actions
(define (do-seq stx actions)
  (for/fold ([s stx])
            ([a actions])
    (runtime-match literals a s)))

; game loop
(let loop ([state initial-state])
  (match state
    [(hash-table ('stx stx)
                 ('transforms transforms)
                 ('messages messages))
     ; output phase
     (pretty-print stx)  
     (pretty-print (first messages))
     ; input phase
     (define key (read))
     ; update state
     (define new-state
       (match key
         ; meta keys
         ['h (hash-set*
              state
              'messages (cons transforms messages))]
         ; BUG for UNDO: do 0 0 0 u x u z
         ; results in 'no-match
         ['z (match transforms
               ['() (hash-set*
                     state
                     'messages (cons "no undo states" messages)
                     )]
               [_ (hash-set*
                   state
                   'messages (cons "reverting to previous state" messages)
                   'stx (do-seq (hash-ref initial-state 'stx)
                                (rest transforms))
                   'transforms (rest transforms))])]
         ; transform keys
         [_ (define transform (get-transform key))
            (match (runtime-match literals transform stx)
              ['no-match state]
              [new-stx (hash-set*
                        state
                        'stx new-stx
                        'transforms (cons transform transforms)
                        'messages (cons "performed action" messages)
                        )])]))
     (loop new-state)]))


; todo: refactor messages
; add a history of messages to hash
; meta actions can generate new messages
; if a message has been added (compare against old)
; then print the new message
; (so that all message display is done at a single site)




