#lang racket

(require "../projects/fructerm/fructerm.rkt"
         "write-in-envs.rkt"
         "../projects/fructerm/f-match.rkt")


; -------------------------------------------------
; SPOOKY GHOST OO STUFF 

; attribute accessors ooooooo
#;(define-syntax-rule (transform-in state 'attr f)
    (match state
      [(hash-table ('attr attr))
       (hash-set state 'attr (f attr))]))

(define-syntax-rule (transform-in state ('attr f) ...)
  ((compose
    (match-lambda
      [(hash-table ('attr attr))
       (hash-set state 'attr (f attr))]) ...)
   state))



(define-syntax-rule (apply-in! object 'attr f)
  (set! object (transform-in object ('attr f))))

; bind a bunch of attributes oooooooo
(define-syntax-rule (define-from state attrs ...)
  (match-define (hash-table ('attrs attrs) ...) state))


; -------------------------------------------------


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

; -------------------------------------------------

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
        ; (my-desugar '(◇ (▹ (sort expr) / ⊙)))
        (mode . nav)
        (transforms . ())
        (messages . ("hello world"))))

(define literals
  #hash((var . ())
        (app . ())
        (λ . ())
        (let . ())
        (|| . ()) ; not sure about this one....
        (♦ . ())
        (◇ . ())
        (▹ . ())
        (▹▹ . ())
        (⊙ . ())
        (expr . ())
        (pat . ())
        (char .())))

(define my-desugar
  (compose (curry restructure literals #hash()) desugar))



; map from keys to functions
(define keymap
  '(["1" ([⋱
            (▹ (sort expr) As ... / ⊙)
            (▹ (sort expr) As ... / 0)])]
    ["2" ([⋱
            (▹ (sort expr) As ... / ⊙)
            ((sort expr) As ... /
                         (app (▹ (sort expr) / ⊙)
                              ((sort expr) / ⊙)))])]
    ["3" ([⋱
            (▹ (sort expr) As ... / ⊙)
            ((sort expr) As ... /
                         (λ ((▹ (sort pat) / ⊙))
                           ((sort expr) / ⊙)))])]
    ["4" ([⋱
            (▹ (sort pat) As ... / ⊙)
            ((sort pat) As ... /
                        (var (▹ (sort char) / ⊙)))]
          [⋱
            (▹ (sort expr) As ...  / ⊙)
            ((sort expr) As ... / (var (▹ (sort char) / ⊙)))])]
    ["up" ([(◇ a ... (▹ As ... / b) c ...)
            (◇ a ... (▹ As ... / b) c ...)]
           [⋱
             (As ... / (λ ((▹ Bs ... / a)) b))
             (▹ As ... / (λ ((Bs ... / a)) b))]
           [⋱
             (As ... / (a ... (▹ Bs ... / b) c ...))
             (▹ As ... / (a ... (Bs ... / b) c ...))]
           )]
    ["down" ([⋱
               (▹ As ... / ⊙)
               (▹ As ... / ⊙)]
             [⋱
               (▹ As ... / 0)
               (▹ As ... / 0)]
             
             [⋱
               (▹ As ... / (var (Bs ... / b)))
               (As ... / (var (▹ Bs ... / b)))]
             [⋱
               (▹ As ... / (app (Bs ... / a) b))
               (As ... / (app (▹ Bs ... / a) b))]
             [⋱
               (▹ As ... / (λ ((Bs ... / a)) b))
               (As ...  / (λ ((▹ Bs ... / a)) b))]
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
            (▹ As ... / 0)
            (▹ As ... / ⊙)]
          [⋱
            (▹ As ... / (var a))
            (▹ As ... / ⊙)]
          [⋱
            (▹ As ... / (app a b))
            (▹ As ... / ⊙)]
          [⋱
            (▹ As ... / (λ (a) b))
            (▹ As ... / ⊙)]
          )]))


(define (get-transform key)
  (let ([res (assoc key keymap)])
    (if res (second res) '([a a]))))

; perform a sequence of actions
(define (do-seq stx actions)
  (for/fold ([s stx])
            ([a actions])
    (runtime-match literals a s)))


(define (mode-text-entry key state)
  (match-define
    (hash-table ('stx stx)) state)
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
     (define inserted-result ; THIS HAS A PROBLEM WITH THE LITERAL a. check pattern-match lib
       (runtime-match (hash-set literals new-value '_) insert stx))
     (println `(inserted ,inserted-result))
     (hash-set*
      state
      'stx inserted-result)]))


(define (mode:navigate key state)
  (define-from state
    stx mode transforms messages)
  (define menu-stx
    (map my-desugar `((♦ (sort expr) / 0) 
                      ((sort expr) / (app ((sort expr) / ⊙) ((sort expr) / ⊙)))
                      #;((sort expr) / (λ (((sort pat) / ⊙)) ((sort expr) / ⊙)))
                      #;((sort expr) / (var ((sort char) / ⊙))))))
  (define update (curry hash-set* state))
  (match key
    ; the new ENTER (transform mode) candidate
    ["q"
     ; here we initially just want to replace a hole with a menu of fill options
     (hash-set* state
                'mode 'menu
                'stx (f/match stx
                       [(c ⋱ (▹ ('sort expr) As ... / ⊙))
                        (c ⋱ (▹ ('template menu-stx) ('sort expr) As ... / ⊙))]
                       ))]
    ; original begin enter text mode
    #;["\r"
       (define (select-first stx)
         (f/match stx
           [`(,x ,xs ...)
            `((▹ ,x) ,@xs)]))
       (f/match stx
         [(c ⋱ (▹ (sort char) in-scope as ... / '⊙))
          (c ⋱ (('string-list (select-first in-scope)) (sort char) in-scope as ... / '⊙))])]
    #;["\r" (define my-transform
              '([(⋱ (▹ (sort char) / ⊙))
                 0]))
            (define my-transform2
              `([⋱ (▹ (sort char) / ⊙)
                   (▹ (sort char) / ||)]))
            (cond [(equal? 0 (runtime-match literals my-transform stx))
                   (begin (println "yeah")
                          (hash-set*
                           state
                           'stx (runtime-match (hash-set literals '|| '_)
                                               my-transform2
                                               stx)
                           'mode 'text-entry))]
                  [else
                   (begin (println "nah")
                          state)]                  
                  )]
    ["f1" (update 'stx save-state-1)]
    ["h"  (update 'messages (cons transforms messages))]
    ; undo last transform
    ; BUG for UNDO: do 0 0 0 u x u z results in 'no-match
    ["z"  (match transforms
            ['() (update 'messages
                         (cons "no undo states" messages))]
            [_ (update 'messages (cons "reverting to previous state" messages)
                       'stx (do-seq (hash-ref initial-state 'stx)
                                    (rest transforms))
                       'transforms (rest transforms))])]
    ; transform keys
    [_ (define transform (get-transform key))
       #;(println transform)
       (match (runtime-match literals transform stx)
         ['no-match state]
         [new-stx (update
                   'stx new-stx
                   'transforms (cons transform transforms)
                   'messages (cons "performed action" messages)
                   )])]))

(define (mode-menu key state)
  (define-from state stx)
  (match key
    ["down"
     (hash-set*
      state
      'stx
      (f/match stx
        [(ctx ⋱ (▹ ('template `(,xs ... ,(♦ y-anns ... / y) ,(z-anns ... / z) ,ws ...))
                   ; note missing rest-anns ...
                   ; above is bug/incomplete-implementation in f-match maybe
                   / s))
         (ctx ⋱ (▹ ('template `(,@xs ,(y-anns ... / y) ,(♦ z-anns ... / z) ,@ws)) ; note missing as above
                   / s))
         ]))]
    ["up"
     (hash-set*
      state
      'stx
      (f/match stx
        [(ctx ⋱ (▹ ('template `(,xs ... ,(y-anns ... / y) ,(♦ z-anns ... / z) ,ws ...))
                   ; note missing rest-anns ...
                   ; above is bug/incomplete-implementation in f-match maybe
                   / s))
         (ctx ⋱ (▹ ('template `(,@xs ,(♦ y-anns ... / y) ,(z-anns ... / z) ,@ws)) ; note missing as above
                   / s))
         ]
        ))]))





#| notes for expr var entry

when an expr var /hole/ is selected, and enter is pressed,
the hole is first replaced by a blank symbol
then we enter text autocomplete mode

when a character is entered, we add that char to the current symbol
if the current symbol doesn't prefix-match against the in-scope list
the symbol should be rendered in red
otherwise, it should be rendered in green

should we allow entry of bad chars period? or just refuse to?

|#


#| notes on transertion searchlection duality

well maybe later

more prosaically for now:

hit enter while hole is selected
if only one thing can fit in that hole, it is inserted
(so need to have list of things that can fill holes of certain sorts)
otherwise, the LIST of things that can be entered is inserted
and we active searchlection mode, but CONFINED to that list
so we naviate the cursor up and down that list
when enter is pressed again, that option replaces the whole list

should we allow navigating into a particular option on the list,
and beginning a 'subtransertion' inside that element?
in other words, if we descended into an element, enter would
not pick the surrounding list element, but rather spawn a new
menu in the hole that was selected.

symbolically:
take transformation rules whose lhs is hole of appropriate type
create list of rhs templates
|#



#| project: stx -> s-expr
   projects cursor and sort info for holes|#
(define (project stx)
  (define @ project)
  (f/match stx
    ; transform mode
    [(▹ template / stx)
     `(▹ [,stx -> ,(project template)])]
    ; label sorts of holes
    [(▹ sort / '⊙)
     `(▹ (⊙ ,sort))]
    [(sort / '⊙)
     `(⊙ ,sort)]
    ; embed cursor
    [(▹ / stx)
     `(▹ ,(@ stx))]
    [(♦ / stx)
     `(♦ ,(@ stx))]
    ; ditch attributes
    [(_ ... / stx)
     (@ stx)]
    [(? list?) (map @ stx)] [x x]))



; mode-loop : key x state -> state
#| determines the effect of key based on mode|#
(define (mode-loop key state)
  (define-from state mode)
  (match mode
    ['text-entry
     (mode-text-entry key state)]
    ['menu
     (mode-menu key state)]
    ['nav
     (mode:navigate key state)]))



; debug-output! : world x state x key -> world
(define (debug-output! state key)
  (match-define
    (hash-table ('stx stx)
                ('mode mode)
                ('transforms transforms)
                ('messages messages)) state)
  (displayln `(mode: ,mode  key: ,key))
  (displayln (pretty-format (project stx)))
  #;(displayln (pretty-format stx))
  #;(displayln (project stx))
  #;(displayln state))


; output : state -> image
(require "layout-lab.rkt")
(define (output state)
  (match state
    [(hash-table ('stx stx))
     #;(render stx)
     (text (pretty-format (project stx))
           24 "black")]))


; MY LOVE FOR YOU IS LIKE A TRUCK
(require 2htdp/image)
(require 2htdp/universe)
(big-bang initial-state
  [on-key
   #| This is impure because of printing debug output.
   and ad-hoc pre-processing. This is where we do
   dirty things. |#
   (λ (state key)
     ; state pre-processors
     (apply-in! state 'stx write-in-envs)
     ; print debugging information
     (debug-output! state key)
     ; transform state based on input and mode
     (mode-loop key state))]
  [to-draw output 800 800])

