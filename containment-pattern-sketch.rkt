#lang racket



; from reddit suggestion by soegaard:
; https://www.reddit.com/r/Racket/comments/8q3h4r/

(define-namespace-anchor here)
(define ns (namespace-anchor->namespace here))

(define (fn xs)
  (define (one? x) (= x 1))
  (define syms (for/list ([i (length xs)]) (string->symbol (~a "x" i))))
  (define args (for/list ([sym syms] [x xs] #:when (one? x)) sym))
  (define body `(list . ,(for/list ([sym syms] [x xs]) (if (one? x) sym 0))))  
  (eval `(lambda ,args ,body) ns))

(require rackunit)
(check-equal? ((fn '(0 0 1 0 1)) 3 4)
              '(0 0 3 0 4))



; containment patterns

; (⋱ <pat>)
; (⋱list <pat> ...)
; (⋱ (until <stop-pat>) <pat>)
; (id ⋱ <pat>)


; version (⋱ pat) read as "contains pat"
#; (check-equal? (tester `(⋱ 1)
                         `(0 1 2 3))
                 #hash())


; by default ⋱ is recursive
#; (check-equal? (tester `(⋱ 1)
                         `(0 (0 1) 2 3))
                 #hash())

; returns the first match
#; (check-equal? (tester `(⋱ 1)
                         `(0 (0 1) 1 3))
                 #hash())

; special symbol if no match
#; (check-equal? (tester `(⋱ 1)
                         `(0 (0 2) 2 3))
                 'no-match)

; specifically: it does a top-down traversal, 
#; (check-equal? (tester `(⋱ (1 a))
                         `(1 (0 (1 3))))
                 #hash((a . (0 (1 3)))))

#; (check-equal? (tester `(⋱ a)
                         `(0 (1 1) 1 3))
                 #hash((a . 0)))

; variant which captures a context
#; (check-equal? (tester `(a ⋱ 1)
                         `(0 1 2 3))
                 #hash((a . (x → (0 x 2 3)))))

#; (check-equal? (tester `(a ⋱ `(0 1 2 3))
                         `(0 1 2 3))
                 #hash((b . `(0 1 2 3))
                       (a . (x → x))))

#; (check-equal? (tester `(a ⋱ b)
                         `(0 1 2 3))
                 #hash((b . `(0 1 2 3))
                       (a . (x → x))))

; should i implement quasiquote before this...
#; (check-equal? (tester `(a ⋱ 'y)
                         `(let ([z 1])
                            (let ([y 2])
                              body)))
                 #hash((a . (x → `(let ([z 1])
                                    (let ([,x 2])
                                      body))))))

#; (check-equal? (tester `(let ([z ,val]) ; z is literal
                            (b ⋱ (until `(let ([z ,_])))
                               z))
                         `(let ([z 1])
                            (let ([z 2])
                              z)
                            (let ([y 3])
                              z)))
                 #hash((val . 1)
                       (b . (_ → `(let ([z 1])
                                    (let ([z 2])
                                      z)
                                    (let ([y 3])
                                      ,_))))))

; coupled with template:
#; `(let ([z ,val])
      (b z))

; would give:
#;`(let ([z 1])
     (let ([z 2])
       z)
     (let ([y 3])
       1))

; hash-pat version of above: (requires == scope change)
#; `((bound-here . (⋱ id)) : let ([idz ,val])
                           (b ⋱ (until ((bound-here . (⋱ idz)) :))
                              idz))


; multi-result ⋱list
#; (check-equal? (tester `(a ⋱ 1)
                         `(0 1 2 1))
                 #hash((a . (x y → `(0 ,x 2 ,y)))))

; one one level:
; get all results from level
; and build up a context accordingly
; the elements that returned 'no-match get included literally
; the elements that returned contexts (so there was a result below), get
; included by composition into the new context
; e.g.
#; (c1 `(0 (0 1) 1 (0 0)))
#; (qq (list (c1 0) (c1 (0 1)) (c1 1) (c1 (0 0))))
#; (qq (list 0 (list (c1 0) (c1 1)) (uq _) (list (c1 0) (c1 0))))
#; (qq (list 0 (list 0 (uq _)) (uq _) (list 0 0)))

; or with real λs:
#;(cl `(0 1))
; folding over list would yield:
#; (list 'no-match (λ (x) x))
; fold again over both. if later is 'no-match, include former.
; want to construct:
#; (λ (x) (list 0 x))
; via
#; (compose (λ (y) (list 0 y)) (λ (x) x))
; what if multiple fn returns?
#;(cl `(0 1 1))
#; (list 'no-match (λ (x) x) (λ (x) x))
#; (λ (a b) (list 0 a b))
;as?
#;(λ (a b) (list 0 (app (λ (x) x) a) (app (λ (y) y) b)))
; probably no good...
; will this work? (multivariate compose):
#; (compose f (values g h))
; no

; list capture version: UNFINISHED
#; (check-equal? (tester `(a ⋱ a-list match-on-list)
                         0)
                 #hash((a . (x → 0))))

; side note: generalizing to searches returning forests?
; like, if we want to continue to descend into matched results
; ...


; side note: hash pattern syntax?
#; (h : rest-sexpr ...) ; bind h to hash
#; (h tag ... : rest-sexpr ...) ; where tag means check (has-has-key tag)
; e.g.
#; (h expr? : if true 0 1)
#; (h (or tag (prop . <pat>)) ... : rest-sexpr ...)
; (destructure (hash-ref h prop) <pat>)
; e.g.
#; (h (type . N) : if true b c)
; binding hash is optional? (actually this makes the syntax ambiguous...)
; find some way to make it work:
#; (tag tag tag : rest ...)
#; ((prop . value) : rest ...)
; in template:
#; (h (prop : (make-new-val value)))