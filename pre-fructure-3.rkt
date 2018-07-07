#lang racket

(require "../projects/fructerm/fructerm.rkt")

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



(define save-state-1
  '(◇ (p/
       #hash((sort . expr) (▹ . ▹))
       (λ ((p/ #hash((sort . expr)) ⊙))
         (p/
          #hash((sort . expr))
          (app
           (p/
            #hash((sort . expr))
            (λ ((p/ #hash((sort . expr)) ⊙)) (p/ #hash((sort . expr)) 0)))
           (p/
            #hash((sort . expr))
            (λ ((p/ #hash((sort . expr)) ⊙))
              (p/
               #hash((sort . expr))
               (app
                (p/ #hash((sort . expr)) ⊙)
                (p/
                 #hash((sort . expr))
                 (app
                  (p/ #hash((sort . expr)) ⊙)
                  (p/ #hash((sort . expr)) 0)))))))))))))



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

(define initial-state
  #hash((stx . (◇ (p/ #hash((▹ . ▹) (sort . expr)) ⊙)))
        (mode . nav)
        (transforms . ())
        (messages . ("hello world"))))

(define literals
  #hash((var . ())
        (app . ())
        (λ . ())
        (let . ())
        #;(pair . ())
        #;(q . ()) ; TEMPORARY!!!
        (◇ . ())
        (▹ . ())
        (▹▹ . ())
        (⊙ . ())
        (expr . ())
        (pat . ())
        (char .())))


; map from keys to functions
(define keymap
  '(["1" ([⋱
           (▹ (sort expr) As ...
              / ⊙)
           (▹ (sort expr) As ...
              / 0)])]
    ["2" ([⋱
           (▹ (sort expr) As ...
              / ⊙)
           ((sort expr) As ...
                        / (app (▹ (sort expr) / ⊙)
                               ((sort expr) / ⊙)))])]
    ["3" ([⋱
           (▹ (sort expr) As ...
              / ⊙)
           ((sort expr) As ...
                        / (λ ((▹ (sort pat) / ⊙))
                            ((sort expr) / ⊙)))])]
    ["4" ([⋱
           (▹ (sort pat) As ...
              / ⊙)
           ((sort pat) As ...
                       / (var (▹ (sort char) / ⊙)))])]
    ["up" ([(◇ a ... (▹ As ... / b) c ...)
            (◇ a ... (▹ As ... / b) c ...)]
           [⋱
            (As ...
             / (λ ((▹ Bs ... / a)) b))
            (▹ As ...
               / (λ ((Bs ... / a)) b))]
           [⋱
            (As ... /
                (a ... (▹ Bs ... / b) c ...))
            (▹ As ... /
               (a ... (Bs ... / b) c ...))]
           )]
    ["down" ([⋱
              (▹ As ... / ⊙)
              (▹ As ... / ⊙)]
             [⋱
              (▹ As ... / 0)
              (▹ As ... / 0)]
             [⋱
              (▹ As ... /
                 (var (Bs ... / b)))
              (As ... /
                  (var (▹ Bs ... / b)))]
             [⋱
              (▹ As ... /
                 (app (Bs ... / a) b))
              (As ... /
                  (app (▹ Bs ... / a) b))]
             [⋱
              (▹ As ...
                 / (λ ((Bs ... / a)) b))
              (As ...
               / (λ ((▹ Bs ... / a)) b))]
             )]
    ["left" ([⋱
              (◇ (▹ As ... / c))
              (◇ (▹ As ... / c))]
             [⋱
              (var (▹ As ... / c))
              (var (▹ As ... / c))]
             [⋱
              (app (▹ As ... / c) d ...)
              (app (▹ As ... / c) d ...)]
             [⋱
              (λ ((▹ Bs ... / a)) b)
              (λ ((▹ Bs ... / a)) b)]
             [⋱
              (λ ((As ... / a)) (▹ Bs ... / b))
              (λ ((▹ As ... / a)) (Bs ... / b))]
             [⋱
              ((▹ As ... / c) d ...)
              ((▹ As ... / c) d ...)]
             [⋱
              (a ... (As ... / b) (▹ Bs ... / c) d ...)
              (a ... (▹ As ... / b) (Bs ... / c) d ...)]
             )]
    ["right" ([⋱
               (λ ((▹ As ... / a)) (Bs ... / b))
               (λ ((As ... / a)) (▹ Bs ... / b))]
              [⋱
               (a ... (▹ As ... / b) (Bs ... / c) d ...)
               (a ... (As ... / b) (▹ Bs ... / c) d ...)]
              )]
    ["x" ([⋱
           (▹ As ...
              / 0)
           (▹ As ...
              / ⊙)]
          [⋱
           (▹ As ...
              / (var a))
           (▹ As ...
              / ⊙)]
          [⋱
           (▹ As ...
              / (app a b))
           (▹ As ...
              / ⊙)]
          [⋱
           (▹ As ...
              / (λ (a) b))
           (▹ As ...
              / ⊙)]
          )]))


(define (get-transform key)
  (let ([res (assoc key keymap)])
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
                 ('mode mode)
                 ('transforms transforms)
                 ('messages messages))
     (println `(mode ,mode))
     (match mode
       ['text-entry
        (match key
          ["\r"
           (hash-set*
            state
            'mode 'nav)]
          [(regexp #rx"[a-z]")
           (define my-transform
             `([⋱ (▹ (sort char) / ||)
                  (▹ (sort char) / ,(string->symbol key))]))
           (define extract
             `([(⋱ (▹ (sort char) / a))
                a]))
           (define extracted-value
             (runtime-match literals extract stx))
           (define new-value
             (string->symbol (string-append (symbol->string extracted-value) key)))
           (println `(extracted ,extracted-value))
           (define insert
             `([⋱ (▹ (sort char) / a)
                  (▹ (sort char) / ,new-value)]))
           (define inserted-result
             (runtime-match (hash-set literals new-value '_) insert stx))
           (println `(inswerted ,inserted-result))
           (hash-set*
            state
            'stx inserted-result)])]
       ['nav
        (match key
          ; meta keys
          ["\r" (define my-transform
                  '([(⋱ (▹ (sort char) / ⊙))
                     0]))
                (define my-transform2
                  `([⋱ (▹ (sort char) / ⊙)
                       (▹ (sort char) / ||)]))
                (if (equal? 0 (runtime-match literals my-transform stx))
                    (begin (println "yeah")
                           (hash-set*
                            state
                            'stx (runtime-match (hash-set literals '|| '_)
                                                my-transform2
                                                stx)
                            'mode 'text-entry))
                    (begin (println "nah")
                           state))]
          ["f1" (hash-set initial-state 'stx save-state-1)]
          ["h"  (hash-set*
                 state
                 'messages (cons transforms messages))]
          ; BUG for UNDO: do 0 0 0 u x u z
          ; results in 'no-match
          ["z"  (match transforms
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
             #;(println transform)
             (match (runtime-match literals transform stx)
               ['no-match state]
               [new-stx (hash-set*
                         state
                         'stx new-stx
                         'transforms (cons transform transforms)
                         'messages (cons "performed action" messages)
                         )])])])]))



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
(big-bang initial-state
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
