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
#;(define initial-state
    #hash((stx . (◇ (▹ (⊙ expr))))
          (transforms . ())
          (messages . ("hello"))))

#; (define initial-state-ann
     #hash((stx . (◇ (p/ ▹ (p/ _ ⊙))) #;(pcons ◇ (pcons (p/ ▹ ⊙) ())))
           (transforms . ())
           (messages . ("hello world"))))

(define initial-state-ann
  #hash((stx . (◇ (p/ #hash((▹ . ▹) (sort . expr)) ⊙)))
        (transforms . ())
        (messages . ("hello world"))))

#;(grammar
   (s (terminal (curry equal? 'expr)))
   (expr (⊙ s)
         (pair expr expr)))

; list of literals (generate from grammar)
#;(define literals
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
  '(["0" ([⋱→
           (▹ (⊙ expr))
           (pair (▹ (⊙ expr)) (⊙ expr))])]
    ["u" ([(◇ a ... (▹ b) c ...)
           (◇ a ... (▹ b) c ...)]
          ; add runtime-match macro for guards like this
          [⋱→
           (a ... (▹ b) c ...)
           (▹ (a ... b c ...))])]
    ["d" ([⋱→
           (▹ (⊙ _))
           (▹ (⊙ _))]
          [⋱→
           (▹ (pair a b))
           (pair (▹ a) b)])]
    ["l" ([⋱→
           (pair (▹ c) d ...)
           (pair (▹ c) d ...)]
          [⋱→
           (a ... b (▹ c) d ...)
           (a ... (▹ b) c d ...)])]
    ["r" ([⋱→
           (a ... (▹ b) c d ...)
           (a ... b (▹ c) d ...)])]
    ["x" ([⋱→
           (▹ (pair a b))
           (▹  (⊙ expr))])]))

#; ( 
    [(? number?)
     (render-datum stx)]
    [(? symbol?)
     (render-identifier stx)]
    [`(app ,f ,as ...)
     (render-call stx)]
    [`(λ ,p ,xs ...)
     (render-λ stx)]
    [`(let ,c ,xs ...)
     (render-let stx)])


(define literals
  #hash((app . ())
        (λ . ())
        (let . ())
        (pair . ())
        (◇ . ())
        (▹ . ())
        (⊙ . ())
        (expr . ())
        (pat . ())))

(define keymap2
  '(["1" ([⋱→
           (▹ (⊙ expr))
           (▹ 0)])]
    ["2" ([⋱→
           (▹ (⊙ expr))
           (app (▹ (⊙ expr)) (⊙ expr))])]
    ["3" ([⋱→
           (▹ (⊙ expr))
           (λ ((▹ (⊙ expr))) (⊙ expr))])]
    ["4" ([⋱→
           (▹ (⊙ expr))
           (let ([(▹ (⊙ expr)) (⊙ expr)]) (⊙ expr))])]
    ["up" ([(◇ a ... (▹ b) c ...)
            (◇ a ... (▹ b) c ...)]
           ; add runtime-match macro for guards like this
           [⋱→
            (λ ((▹ a)) b)
            (▹ (λ (a) b))]
           [⋱→
            (let ([(▹ a) b]) c)
            (▹ (let ([a b]) c))]
           [⋱→
            (let ([a (▹ b)]) c)
            (▹ (let ([a b]) c))]
           [⋱→
            (a ... (▹ b) c ...)
            (▹ (a ... b c ...))])]
    ["down" ([⋱→
              (▹ (⊙ _))
              (▹ (⊙ _))]
             [⋱→
              (▹ 0)
              (▹ 0)]
             [⋱→
              (▹ (app a b))
              (app (▹ a) b)]
             [⋱→
              (▹ (λ (a) b))
              (λ ((▹ a)) b)]
             [⋱→
              (▹ (let ([a b]) c))
              (let ([(▹ a) b]) c)])]
    ["left" ([⋱→
              (◇ (▹ c))
              (◇ (▹ c))]
             [⋱→
              (λ (a) (▹ b))
              (λ ((▹ a)) b)]
             [⋱→
              (let ([a b]) (▹ c))
              (let ([a (▹ b)]) c)]
             [⋱→
              (app (▹ c) d ...)
              (app (▹ c) d ...)]
             [⋱→
              ((▹ c) d ...)
              ((▹ c) d ...)]
             [⋱→
              (a ... b (▹ c) d ...)
              (a ... (▹ b) c d ...)])]
    ["right" ([⋱→
               (λ ((▹ a)) b)
               (λ (a) (▹ b))]
              [⋱→
               (let ([(▹ a) b]) c)
               (let ([a (▹ b)]) c)]
              [⋱→
               (let ([a (▹ b)]) c)
               (let ([a b]) (▹ c))]
              [⋱→
               (a ... (▹ b) c d ...)
               (a ... b (▹ c) d ...)])]
    ["x" ([⋱→
           (▹ 0)
           (▹ (⊙ expr))]
          [⋱→
           (▹ (app a b))
           (▹ (⊙ expr))]
          [⋱→
           (▹ (λ (a) b))
           (▹ (⊙ expr))]
          [⋱→
           (▹ (let ([a b]) c))
           (▹ (⊙ expr))])]))


(define keymap-ann-before-remainders
  '(["1" ([⋱→
           (▹ / ⊙)
           (▹ / 0)])]
    ["2" ([⋱→
           (▹ / ⊙)
           ( /
             (app (▹ / ⊙) ( / ⊙)))])]
    ["up" ([(◇ a ... (▹ / b) c ...)
            (◇ a ... (▹ / b) c ...)]
           [⋱→
            ( / (a ... (▹ / b) c ...))
            (▹ / (a ... ( / b) c ...))]
           )]
    ["down" ([⋱→
              (▹ / ⊙)
              (▹ / ⊙)]
             [⋱→
              (▹ / 0)
              (▹ / 0)]
             [⋱→
              (▹ /
                 (app ( / a) b))
              ( /
                (app (▹ / a) b))]
             )]
    ["left" ([⋱→
              (◇ (▹ / c))
              (◇ (▹ / c))]
             [⋱→
              (app (▹ / c) d ...)
              (app (▹ / c) d ...)]
             [⋱→
              ((▹ / c) d ...)
              ((▹ / c) d ...)]
             [⋱→
              (a ... ( / b) (▹ / c) d ...)
              (a ... (▹ / b) ( / c) d ...)]
             )]
    ["right" ([⋱→
               (a ... (▹ / b) ( / c) d ...)
               (a ... ( / b) (▹ / c) d ...)]
              )]
    ["x" ([⋱→
           (▹ / 0)
           (▹ / ⊙)]
          [⋱→
           (▹ / (app a b))
           (▹ / ⊙)]
          )]))

(define keymap-ann
  '(["1" ([⋱→
           (▹ (sort expr) As ...
              / ⊙)
           (▹ (sort expr) As ...
              / 0)])]
    ["2" ([⋱→
           (▹ (sort expr) As ...
              / ⊙)
           ((sort expr) As ...
                        / (app (▹ (sort expr) / ⊙)
                               ((sort expr) / ⊙)))])]
    ["up" ([(◇ a ... (▹ As ... / b) c ...)
            (◇ a ... (▹ As ... / b) c ...)]
           [⋱→
            (As ... /
                (a ... (▹ Bs ... / b) c ...))
            (▹ As ... /
               (a ... (Bs ... / b) c ...))]
           )]
    ["down" ([⋱→
              (▹ As ... / ⊙)
              (▹ As ... / ⊙)]
             [⋱→
              (▹ As ... / 0)
              (▹ As ... / 0)]
             [⋱→
              (▹ As ... /
                 (app (Bs ... / a) b))
              (As ... /
                  (app (▹ Bs ... / a) b))]
             )]
    ["left" ([⋱→
              (◇ (▹ As ... / c))
              (◇ (▹ As ... / c))]
             [⋱→
              (app (▹ As ... / c) d ...)
              (app (▹ As ... / c) d ...)]
             [⋱→
              ((▹ As ... / c) d ...)
              ((▹ As ... / c) d ...)]
             [⋱→
              (a ... (As ... / b) (▹ Bs ... / c) d ...)
              (a ... (▹ As ... / b) (Bs ... / c) d ...)]
             )]
    ["right" ([⋱→
               (a ... (▹ As ... / b) (Bs ... / c) d ...)
               (a ... (As ... / b) (▹ Bs ... / c) d ...)]
              )]
    ["x" ([⋱→
           (▹ As ...
              / 0)
           (▹ As ...
              / ⊙)]
          [⋱→
           (▹ As ...
              / (app a b))
           (▹ As ...
              / ⊙)]
          )]))


(define (get-transform key)
  (let ([res (assoc key keymap-ann)])
    (if res (second res) '([a a]))))

; perform a sequence of actions
(define (do-seq stx actions)
  (for/fold ([s stx])
            ([a actions])
    (runtime-match literals a s)))

; game loop
(define (loop key state)
  #;(displayln state)
  (match state
    [(hash-table ('stx stx)
                 ('transforms transforms)
                 ('messages messages))
     (match key
       ; meta keys
       ["h" (hash-set*
             state
             'messages (cons transforms messages))]
       ; BUG for UNDO: do 0 0 0 u x u z
       ; results in 'no-match
       ["z" (match transforms
              ['() (hash-set*
                    state
                    'messages (cons "no undo states" messages)
                    )]
              [_ (hash-set*
                  state
                  'messages (cons "reverting to previous state" messages)
                  'stx (do-seq (hash-ref initial-state-ann 'stx)
                               (rest transforms))
                  'transforms (rest transforms))])]
       ; transform keys
       [_ (define transform (get-transform key))
          #;(println transform)
          (match (runtime-match literals transform stx)
            ['no-match state]
            [new-stx (hash-set*
                      state
                      'stx new-stx
                      'transforms (cons transform transforms)
                      'messages (cons "performed action" messages)
                      )])])]))



; project cursor and sort info for holes
(define (project stx)
  (define @ project)
  (match stx
    [`(p/ ▹ ,stx)
     `(▹ ,(@ stx))]
    [`(p/ ,(and (hash-table ('sort sort))
                (hash-table ('▹ _))) ⊙)
     `(▹ (⊙ ,sort))]
    [`(p/ ,(hash-table ('sort sort)) ⊙)
     `(⊙ ,sort)]
    [`(p/ ,(hash-table ('▹ _)) ,stx)
     `(▹ ,(@ stx))]
    [`(p/ ,ann ,stx)
     (@ stx)]
    [(? list?) (map @ stx)]
    [x x]))





(require "layout-lab.rkt")

; REAL-TIME
(require 2htdp/image)
(require 2htdp/universe)
(big-bang initial-state-ann
  [on-key
   (λ (state key)
     (match state
       [(hash-table ('stx stx))
        (displayln key)
        (displayln (pretty-format stx))
        (displayln (project stx))])
     (loop key state))]
  [to-draw
   (λ (state)
     (match state
       [(hash-table ('stx stx))
        (render stx)
        #;(text (pretty-format (project stx))
              24 "black")])) 800 800])


#| the following test indicates that my rewriting rule
   here performs exponentially in the depth of the
   first match|#

#; (for/list ([i 20])(time (do-seq (match initial-state
                                     [(hash-table ('stx stx))
                                      stx]) (make-list i '([⋱→
                                                            (▹ (⊙ expr))
                                                            (pair (▹ (⊙ expr)) (⊙ expr))])))))

