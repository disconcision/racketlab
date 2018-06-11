#lang racket

(require rackunit)
(require racket/hash)

; gonna try to reproduce racket/match ellipses matching
; so i can use ellipses-based list patterns in my run-time matcher

; recall that match ellipses are greedy:

(check-equal?
 (match '(1 1 1 1 1 2 1 1 1 1 2)
   [`(,a ... 2 ,b ...)
    a])
 '(1 1 1 1 1 2 1 1 1 1))

(check-equal?
 (match '(1 1 1 1 1 2 1 1 1 1 3)
   [`(,a ... 2 ,b ...)
    a])
 '(1 1 1 1 1))

(check-equal?
 (match '(2 1 1 1 1 2 1 1 1 1 2)
   [`(,a ... 2 ,b ...)
    a])
 '(2 1 1 1 1 2 1 1 1 1))

(check-equal?
 (match '(1 1 1 1 1)
   [`(,a ... 1 ,b ...)
    a])
 '(1 1 1 1))


; to simply the problem i'm considering a rewriting stage:

; i define a new pattern form;
#; (p... <init-pat> <rest-pat>)

#; `(,a ... 2 ,b ...)
; would be equivalent to:
#; `(p... `(,a ... 2) `(,b ...))
#; `(p... `(,a ... 2) b) ; where b is list
; parsing associativity options:
#; `(p... (p... a '(2)) b) ; where a, b are lists
; or:
#; `(p... a (p... '(2) b)) ; where a, b are lists
; the later suggests:

#|
bind a to list contining first element, then try to match 2nd arg (rest of pattern) to
rest of list. if suceed, call binding of a last-success, and rest of list last-rest
then try to bind a to list containing first two elements, etc, until the end of the list
we're matching against. then we return the last-success binding of a, unioned with
the bindings from applying the rest of the pattern to last-rest.

or: start from the end, and work backwards? (this is what i end up doing)
|#

; what do we need to forbid?
#; (p... a (p... b whatever))

; define the following as well:
#; (pcons <init-pat> <rest-pat>) ; where pa is just p-cons
#; (pl* <init-pats> ... <rest-pat>) ; like list*

; so the following are equivalent:
#; `(A B C ,d ... E ,f ... G H) 
#; `(pcons A (pcons B (pcons C (p... ,d (pcons E (p... ,f (pcons G (H))))))))
#; `(pl* A B C (p... d (pl* E (p... f (pl* G H)))))


; here is a rewriter for the above:
; we'll also make some other patterns explicit

(define (desugar-pattern stx)
  (define D desugar-pattern)
  (match stx
    ; containment patterns
    [`(⋱ ,pat)
     (D `(p⋱ _ ,pat))]
    [`(⋱ (until ,?-pat) ,pat)
     (D `(p⋱until _ ,?-pat ,pat))]
    [`(,id ⋱ ,pat)
     (D `(p⋱ ,id ,pat))]
    [`(,id ⋱ (until ,?-pat) ,pat)
     (D `(p⋱until ,id ,?-pat ,pat))]
    
    ; lists and ellipses patterns
    [`(,(and (not 'p⋱)
             (not 'p⋱until)
             (not 'p...)
             (not 'pcons)) . ,_)
     (D (foldr
         (λ (x acc)
           (match acc
             [`(,_ ,(== '...) ,wh ...) `(p... ,x ,(first wh))]
             [_ `(pcons ,x ,acc)]))
         '() stx))]

    ; budget recursion scheme
    [(? list?) (map D stx)]
    [_ stx]))


(define (desugar-template stx)
  (define D desugar-pattern)
  (match stx
    [`(,id ⋱ ,pat)
     (D `(p⋱ ,id ,pat))]
    [`(,(and (not 'p⋱)
             (not 'p...)
             (not 'pcons)) . ,_)
     (D (foldr
         (λ (x acc)
           (match acc
             [`(,_ ,(== '...) ,y ,ys ...)
              `(p... ,x ,y)]
             [_
              `(pcons ,x ,acc)]))
         '() stx))]
    [(? list?) (map D stx)]
    [_ stx]))

; some tests
(check-equal? (desugar-pattern `(a ⋱ 1))
              '(p⋱ a 1))

(check-equal? (desugar-pattern `(a ⋱ (until 2) 1))
              '(p⋱until a 2 1))

(check-equal? (desugar-pattern `(a ⋱ (1 b ...)))
              '(p⋱ a (pcons 1 (p... b ()))))

(check-equal? (desugar-pattern '(1 2 3 d ... 5 f ... 7 8))
              '(pcons 1 (pcons 2 (pcons 3 (p... d (pcons 5 (p... f (pcons 7 (pcons 8 ())))))))))




; helpers

(define (bind x f)
  (if (equal? 'no-match x)
      'no-match
      (f x)))

(define (append-hashes h1 h2)
  (hash-union
   h1
   ; must be a better way to do below:
   (make-hash (hash-map h2 (λ (k v) (cons k (list v)))))
   #:combine (λ (a b) (append a b))))


; the match algorithm

(define (destructure types c-env arg pat)
  #;(println "pm")
  #;(println arg)
  #;(println pat)
  
  (define (accumulate-matches pat x acc)
    (bind acc
          (λ (_)
            (bind (destructure types #hash() x pat)
                  (curry append-hashes acc)))))
  (define constructor-id?
    (curry hash-has-key? types))
  (define literal?
    (disjoin number? constructor-id?))
  (define pattern-variable?
    (conjoin symbol? (negate literal?)))
  (define D (curry destructure types c-env))
  (match* (pat arg)
    [((? literal?)
      (== pat))
     c-env]
    [((? pattern-variable?)
      _)
     (hash-set c-env pat arg)] 
    [(`(p⋱ ,cntx-name ,(app (curry D arg) (? hash? new-env)))
      _)
     (hash-union new-env (hash-set c-env cntx-name identity))]
    [(`(p⋱ ,cntx-name ,find-pat)
      `(,xs ...))

     ; decompose arg list
     (define-values (init term)
       (splitf-at xs
                  (λ (x)
                    (not (hash? (D x `(p⋱ ,cntx-name ,find-pat)))))))

     (match init
       [(== xs) 'no-match]
       [_ (define new-env (D (first term) `(p⋱ ,cntx-name ,find-pat)))
          (define sub-fn (hash-ref new-env cntx-name))
          (hash-set new-env cntx-name
                    (compose (λ (x) `(,@init ,x ,@(rest term)))
                             sub-fn))])

     ]
    [(`(pcons ,first-pat ,rest-pat)
      `(,first-arg ,rest-arg ...))
     (bind (D (first arg) first-pat)
           (λ (h) (bind (D rest-arg rest-pat)
                        (curry hash-union h))))]
    [(`(p... ,p ,ps)
      `(,as ...))
     (define/match (greedy arg-init arg-tail)
       [('() _)
        (bind (D arg-tail ps)
              (bind (D `() p)
                    (curry hash-union)))]
       [(`(,as ... ,b) `(,cs ...))
        (match (D cs ps)
          ['no-match (greedy as `(,b ,@cs))]
          [new-env
           (match (foldl (curry accumulate-matches p)
                         #hash() `(,@as ,b))
             ['no-match (greedy as `(,b ,@cs))]
             [old-env (hash-union new-env old-env)])])])
     (greedy arg '())]
    [(`(,ps ...) `(,as ...))
     #:when (equal? (length ps) (length as))
     (foldl (λ (arg pat env)
              (destructure types env arg pat))
            c-env arg pat)]
    [(_ _) 'no-match]))



; TESTS

(define pm-test (λ (source pattern)
                  (destructure (hash) (hash)
                               source (desugar-pattern pattern))))

; tests for pl pattern form

(check-equal? (pm-test '() '(pcons () anything))
              'no-match)

(check-equal? (pm-test '(1) '(pcons 1 ()))
              #hash())

(check-equal? (pm-test  '(1) '(pcons a ()))
              #hash((a . 1)))

(check-equal? (pm-test '(1 2) '(pcons 1 (2)))
              #hash())

(check-equal? (pm-test '(1 2) '(pcons 1 (pcons 2 ())))
              #hash())

(check-equal? (pm-test '(1 2 3) '(pcons a (2 3)))
              #hash((a . 1)))

(check-equal? (pm-test '(1 2 3) '(pcons 1 (pcons 2 (pcons 3 ()))))
              #hash())


; tests for pa pattern form

(check-equal? (pm-test '() '(p... a ()))
              #hash((a . ())))

(check-equal? (pm-test '(1) '(p... a ()))
              #hash((a . (1))))

(check-equal? (pm-test '(1 2) '(p... a (pcons 2 ())))
              #hash((a . (1))))


; pa with non-wildcard patterns

(check-equal? (pm-test
               '((1 1) (1 2) (4 5) 3 4)
               '(p... (a b) (3 4)))
              #hash((a . (1 1 4)) (b . (1 2 5))))


; ellipses pattern tests



; FIX THIS ISSUE!!!!!
#; (check-equal? (pm-test
                  '()
                  '(1 ...))
                 #hash())

(check-equal? (pm-test
               '(1)
               '(2 ...))
              'no-match)

(check-equal? (pm-test
               '(a)
               '(1 ...))
              'no-match)

(check-equal? (pm-test
               '(1)
               '(1 ...))
              #hash())

(check-equal? (pm-test
               '()
               '(f ...))
              #hash((f . ())))

(check-equal? (pm-test
               '(1)
               '(f ...))
              #hash((f . (1))))

(check-equal? (pm-test
               '(1)
               '(1 f ...))
              #hash((f . ())))

(check-equal? (pm-test
               '(1 2 3 4 5)
               '(1 2 d ...))
              '#hash((d . (3 4 5))))

(check-equal? (pm-test
               '(1 2 3 4 5)
               '(1 2 3 d ... 5))
              '#hash((d . (4))))

(check-equal? (pm-test
               '(1 2 3 4 5)
               '(a ... 2 b ... 5))
              #hash((a . (1)) (b . (3 4))))

(check-equal? (pm-test
               '(1 2 3 4 5)
               '(1 a ... 2 b ... 5))
              #hash((a . ()) (b . (3 4))))

(check-equal? (pm-test
               '(1 2 3 4 5)
               '(1 2 3 d ... 5 f ...))
              #hash((f . ()) (d . (4))))

(check-equal? (pm-test
               '(1 2 3 4 5 6)
               '(1 2 3 d ... 5 f ... 6))
              #hash((f . ()) (d . (4))))

(check-equal? (pm-test
               '(1 2 3 4 5 6 7)
               '(1 2 3 d ... 5 f ... 7))
              #hash((f . (6)) (d . (4))))

(check-equal? (pm-test
               '(1 2 3 4 4 5 6 6 6 7 8)
               '(1 2 3 d ... 5 f ... 7 8))
              #hash((f . (6 6 6)) (d . (4 4))))


; complex pattern in ellipses tests

(check-equal? (pm-test
               '(1 (4 1 (6 2)) (4 3 (6 4)))
               '(1 (4 a (6 b)) ...))
              #hash((a . (1 3)) (b . (2 4))))


; nested ellipses tests

(check-equal? (pm-test
               '(1 (1 2 3) (4 5 6))
               '(1 (a ...) ...))
              #hash((a . ((1 2 3) (4 5 6)))))

(check-equal? (pm-test
               '(1 (1 2 3) (1 5 6))
               '(1 (1 a ...) ...))
              #hash((a . ((2 3) (5 6)))))

(check-equal? (pm-test
               '(1 (1 0 0 0 1 0 0) (1 0 1 0 0 0))
               '(1 (1 a ... 1 b ...) ...))
              #hash((a . ((0 0 0) (0)))
                    (b . ((0 0) (0 0 0)))))


#|
side note: for explaining pattern matching; the need to distiguish
datums/literals from pattern variables: take the analogy of expanding
a fn in source code. looking for a fn call, the fn name is a literal
and the parameters are captured as pattern variable (modulo evaluation
strategy of course; lead into discussion of macros)
|#

(define (restructure types env stx)
  (define (constructor-id? id)
    (hash-has-key? types id))
  (define I (curry restructure types env))
  (match stx
    ['() '()]
    [(? number? n) n]
    [(? symbol? id) (hash-ref env id)]
    [`(p⋱ ,(? symbol? id) ,arg)
     ((hash-ref env id) (I arg))]
    [`(pcons ,p ,ps)
     (cons (I p) (I ps))]
    [`(p... ,p ,ps)
     (append (I p) (I ps))]))


(define (runtime-match types pat-tems source)
  (match pat-tems
    [`() 'no-match]
    [`((,(app desugar-pattern pattern)
        ,(app desugar-template template))
       ,other-clauses ...)
     (define env (destructure types #hash() source pattern))
     (if (equal? 'no-match env)
         (runtime-match types other-clauses source)
         (restructure types env template))]))

(check-equal? (runtime-match #hash() '((a 2)) '1)
              2)

(check-equal? (runtime-match #hash() '(((a b) b)) '(1 2))
              2)

(check-equal? (runtime-match #hash() '(((a ...) (a ...))) '(1 2))
              '(1 2))


(check-equal? (runtime-match #hash() '(((a b ...) (b ... (a)))) '(1 2 3))
              '(2 3 (1)))

(check-equal? (runtime-match #hash() '(((a b ...) (b ... (a)))) '(1))
              '((1)))

(define-syntax-rule (ratch source clauses ...)
  (runtime-match #hash() '(clauses ...) source))

(check-equal? (ratch '(let ([a 1] [b 2]) 0)
                     [(form ([id init] ...) body)
                      (id ... init ...)])
              (match '(let ([a 1] [b 2]) 0)
                [`(,form ([,id ,init] ...) ,body)
                 `(,@id ,@init)]))


(define contain-test (λ (source pattern)
                       (destructure (hash) (hash)
                                    source (desugar-pattern pattern))))


(check-equal? ((hash-ref (contain-test
                          '(1)
                          '(a ⋱ 1))
                         'a)
               1)
              '(1))

(check-equal? ((hash-ref (contain-test
                          '(1 0)
                          '(a ⋱ 1))
                         'a)
               1)
              '(1 0))

(check-equal? ((hash-ref (contain-test
                          '(0 1)
                          '(a ⋱ 1))
                         'a)
               1)
              '(0 1))

(check-equal? ((hash-ref (contain-test
                          '(0 1 0)
                          '(a ⋱ 1))
                         'a)
               1)
              '(0 1 0))

(check-equal? ((hash-ref (contain-test
                          '(0 (0 1) 0)
                          '(a ⋱ 1))
                         'a)
               1)
              '(0 (0 1) 0))

; should this be valid? or should containment patterns only match lists??
(check-equal? ((hash-ref (contain-test
                          1
                          '(a ⋱ 1))
                         'a)
               2)
              2)

(check-equal? (hash-ref (contain-test
                         1
                         '(a ⋱ b))
                        'b)
              1)

(check-equal? ((hash-ref (contain-test
                          '(0 2)
                          '(a ⋱ (0 b)))
                         'a)
               '(0 3))
              '(0 3))

(check-equal? ((hash-ref (contain-test
                          '(0 (0 2) 0)
                          '(a ⋱ (0 b)))
                         'a)
               '(0 3))
              '(0 (0 3) 0))



(check-equal? (runtime-match #hash()
                             '(((a ⋱ 1)
                                (a ⋱ 2)))
                             '(1))
              '(2))

(check-equal? (runtime-match #hash()
                             '(((a ⋱ 1)
                                (a ⋱ 2)))
                             '(0 1))
              '(0 2))

(check-equal? (runtime-match #hash()
                             '(((a ⋱ 1)
                                (a ⋱ 2)))
                             '(0 (0 0 1) 0))
              '(0 (0 0 2) 0))

(check-equal? (runtime-match #hash()
                             '(((a ⋱ (0 b))
                                (a ⋱ (1 b))))
                             '(0 (0 2) 0))
              '(0 (1 2) 0))





