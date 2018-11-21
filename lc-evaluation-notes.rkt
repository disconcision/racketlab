#lang racket



; select stx
; hit subst button
; prompts for var-name
; this is transform-mode on (id (hole char))
; hit left
; prompts for new-val
; this is (hole expr)

(define (subs stx old-var new-val)
  (define S (curryr subs old-var new-val))
  (match stx
    [`(ref ,(== old-var)) new-val]
    [`(ref ,y) stx]
    [`(app ,f ,a) `(app ,(S f) ,(S a))]
    [`(λ (,(== old-var)) ,b)
     stx]
    ; old-var !in free(b)
    [`(λ (,y) ,b)
     #; condition
     stx]
    ; y !in free(new-val)
    [`(λ (,y) ,b)
     #; condition
     `(λ (,y) ,(S b))]
    ; old-var in free(b)
    ; and y in free(new-val)
    [`(λ (,y) ,b)
     #; condition
     (define z (gensym))
     ; z !in b & !in new-val
     `(λ (,z) (S (subs b y z)))]
    ))