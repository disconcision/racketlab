#lang racket

(require rackunit)

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
#; (pa <init-pat> <rest-pat>)

#; `(,a ... 2 ,b ...)
; would be equivalent to:
#; `(pa `(,a ... 2) `(,b ...))
#; `(pa `(,a ... 2) b) ; where b is list
; parsing associativity options:
#; `(pa (pa a '(2)) b) ; where a, b are lists
; or:
#; `(pa a (pa '(2) b)) ; where a, b are lists
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
#; (pa a (pa b whatever))

; define the following as well:
#; (pl <init-pat> <rest-pat>) ; where pa is just p-cons
#; (pl* <init-pats> ... <rest-pat>) ; like list*

; so the following are equivalent:
#; `(A B C ,d ... E ,f ... G H) 
#; `(pl A (pl B (pl C (pa ,d (pl E (pa ,f (pl G (H))))))))
#; `(pl* A B C (pa d (pl* E (pa f (pl* G H)))))


; here is a rewriter for the above:
(define (parse-ellipses-pattern stx)
  (foldr
   (λ (x acc)
     (if (and (not (empty? acc))
              (equal? (second acc) '...))
         `(pa ,(if (list? x) (parse-ellipses-pattern x) x) ,(third acc))
         `(pl ,(if (list? x) (parse-ellipses-pattern x) x) ,acc)))
   '() stx))


; some tests
(check-equal? (parse-ellipses-pattern '(1 2 3 d ... 5 f ... 7 8))
              '(pl 1 (pl 2 (pl 3 (pa d (pl 5 (pa f (pl 7 (pl 8 ())))))))))


; the match algorithm
(require racket/hash)
(define (pattern-match types c-env arg pat)
  #;(println "pm")
  #;(println arg)
  #;(println pat)
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
  (define (accumulate-over-seg pat x acc)
    (bind acc
          (λ (_)
            (bind (pattern-match types #hash() x pat)
                  (curry append-hashes acc)))))
  (define constructor-id?
    (curry hash-has-key? types))
  (define Pm (curry pattern-match types c-env))
  (match* (pat arg)
    [((or (? number?) (? (constructor-id?)))
      (== pat))
     c-env]
    [((and (? symbol?) (not (? constructor-id?)))
      _)
     (hash-set c-env pat arg)]
    [(`(pl ,first-pat ,rest-pat)
      `(,first-arg ,rest-arg ...))
     (bind (Pm (first arg) first-pat)
           (bind (Pm rest-arg rest-pat)
                 (curry hash-union)))]
    [(`(pa ,p ,ps)
      `(,as ...))
     (define/match (greedy arg-init arg-tail)
       [('() _)
        (bind (Pm arg-tail ps)
              (bind (Pm `() p)
                    (curry hash-union)))]
       [(`(,as ... ,b) `(,cs ...))
        (match (Pm cs ps)
          ['no-match (greedy as `(,b ,@cs))]
          [new-env
           (match (foldl (curry accumulate-over-seg p)
                         #hash() `(,@as ,b))
             ['no-match (greedy as `(,b ,@cs))]
             [old-env (hash-union new-env old-env)])])])
     (greedy arg '())]
    [(`(,ps ...) `(,as ...))
     #:when (equal? (length ps) (length as))
     (foldl (λ (arg pat env)
              (pattern-match types env arg pat))
            c-env arg pat)]
    [(_ _) 'no-match]))



; TESTS

(define pm-test (curry pattern-match (hash) (hash)))


; tests for pl pattern form

(check-equal? (pm-test '() '(pl () anything))
              'no-match)

(check-equal? (pm-test '(1) '(pl 1 ()))
              #hash())

(check-equal? (pm-test  '(1) '(pl a ()))
              #hash((a . 1)))

(check-equal? (pm-test '(1 2) '(pl 1 (2)))
              #hash())

(check-equal? (pm-test '(1 2) '(pl 1 (pl 2 ())))
              #hash())

(check-equal? (pm-test '(1 2 3) '(pl a (2 3)))
              #hash((a . 1)))

(check-equal? (pm-test '(1 2 3) '(pl 1 (pl 2 (pl 3 ()))))
              #hash())


; tests for pa pattern form

(check-equal? (pm-test '() '(pa a ()))
              #hash((a . ())))

(check-equal? (pm-test '(1) '(pa a ()))
              #hash((a . (1))))

(check-equal? (pm-test '(1 2) '(pa a (pl 2 ())))
              #hash((a . (1))))


; pa with non-wildcard patterns

(check-equal? (pm-test
               '((1 1) (1 2) (4 5) 3 4)
               '(pa (a b) (3 4)))
              #hash((a . (1 1 4)) (b . (1 2 5))))


; ellipses pattern tests



; FIX THIS ISSUE!!!!!
#;
(check-equal? (pm-test
               '()
               (parse-ellipses-pattern '(1 ...)))
              #hash())

(check-equal? (pm-test
               '(1)
               (parse-ellipses-pattern '(2 ...)))
              'no-match)

(check-equal? (pm-test
               '(a)
               (parse-ellipses-pattern '(1 ...)))
              'no-match)

(check-equal? (pm-test
               '(1)
               (parse-ellipses-pattern '(1 ...)))
              #hash())

(check-equal? (pm-test
               '()
               (parse-ellipses-pattern '(f ...)))
              #hash((f . ())))

(check-equal? (pm-test
               '(1)
               (parse-ellipses-pattern '(f ...)))
              #hash((f . (1))))

(check-equal? (pm-test
               '(1)
               (parse-ellipses-pattern '(1 f ...)))
              #hash((f . ())))

(check-equal? (pm-test
               '(1 2 3 4 5)
               (parse-ellipses-pattern '(1 2 d ...)))
              '#hash((d . (3 4 5))))

(check-equal? (pm-test
               '(1 2 3 4 5)
               (parse-ellipses-pattern '(1 2 3 d ... 5)))
              '#hash((d . (4))))

(check-equal? (pm-test
               '(1 2 3 4 5)
               (parse-ellipses-pattern '(a ... 2 b ... 5)))
              #hash((a . (1)) (b . (3 4))))

(check-equal? (pm-test
               '(1 2 3 4 5)
               (parse-ellipses-pattern '(1 a ... 2 b ... 5)))
              #hash((a . ()) (b . (3 4))))

(check-equal? (pm-test
               '(1 2 3 4 5)
               (parse-ellipses-pattern '(1 2 3 d ... 5 f ...)))
              #hash((f . ()) (d . (4))))

(check-equal? (pm-test
               '(1 2 3 4 5 6)
               (parse-ellipses-pattern '(1 2 3 d ... 5 f ... 6)))
              #hash((f . ()) (d . (4))))

(check-equal? (pm-test
               '(1 2 3 4 5 6 7)
               (parse-ellipses-pattern '(1 2 3 d ... 5 f ... 7)))
              #hash((f . (6)) (d . (4))))

(check-equal? (pm-test
               '(1 2 3 4 4 5 6 6 6 7 8)
               (parse-ellipses-pattern '(1 2 3 d ... 5 f ... 7 8)))
              #hash((f . (6 6 6)) (d . (4 4))))


; complex pattern in ellipses tests

(check-equal? (pm-test
               '(1 (4 1 (6 2)) (4 3 (6 4)))
               (parse-ellipses-pattern '(1 (4 a (6 b)) ...)))
              #hash((a . (1 3)) (b . (2 4))))


; nested ellipses tests

(check-equal? (pm-test
               '(1 (1 2 3) (4 5 6))
               (parse-ellipses-pattern '(1 (a ...) ...)))
              #hash((a . ((1 2 3) (4 5 6)))))

(check-equal? (pm-test
               '(1 (1 2 3) (1 5 6))
               (parse-ellipses-pattern '(1 (1 a ...) ...)))
              #hash((a . ((2 3) (5 6)))))

(check-equal? (pm-test
               '(1 (1 0 0 0 1 0 0) (1 0 1 0 0 0))
               (parse-ellipses-pattern '(1 (1 a ... 1 b ...) ...)))
              #hash((a . ((0 0 0) (0)))
                    (b . ((0 0) (0 0 0)))))


#|
side note: for explaining pattern matching; the need to distiguish
datums/literals from pattern variables: take the analogy of expanding
a fn in source code. looking for a fn call, the fn name is a literal
and the parameters are captured as pattern variable (modulo evaluation
strategy of course; lead into discussion of macros)
|#

