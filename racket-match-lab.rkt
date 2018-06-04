#lang racket

(require rackunit)

; match ellipses are greedy:

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




#; `(,a ... 2 ,b ...)
#; `(p-append `(,a ... 2) `(,b ...))
#; `(p-append `(,a ... 2) b) ; where b is list
#; `(p-append a '(2) b) ; where a, b are lists
#; `(p-append (p-append a '(2)) b) ; where a, b are lists
; or:
#; `(p-append a (p-append '(2) b)) ; where a, b are lists
; the later suggests:
#|
bind a to list contining first element, then try to match 2nd arg (rest of pattern) to
rest of list. if suceed, call binding of a last-success, and rest of list last-rest
then try to bind a to list containing first two elements, etc, until the end of the list
we're matching against. then we return the last-success binding of a, unioned with
the bindings from applying the rest of the pattern to last-rest

|#

; what do we need to forbid?
#; (p-append a b) ; for lists a, b (what if p not a list pat?)
#; (p-append a (p-append b whatever)) ; for lists a, b


#|
first, lets rewrite ...-containing variable-length list patterns:
pc = pcons: operates like cons  (first elem is elem, second elem is list)
pa = pappend: 
|#
#; `(A B C ,d ... E ,f ... G H) 
#; `(pl A (pl B (pl C (pa ,d (pl E (pa ,f (pl G (H))))))))
; list* style alernative parsing:
#; `(pl* A B C (pa d (pl* E (pa f (pl* G H)))))

; or other way?:
#; `(p-append (p-append a '(2)) b)
#; `(pl (pl (pa (pl (pa (pl (pl A (B)) C) ,d) E) ,f) G) H)
#; `(*pl (pa (*pl (pa (*pl A B C) ,d) E) ,f) G H)
; maybe should just reverse the list to start... lesser of evils?


(define (parse-ellipses-pattern stx)
  (foldr
   (λ (x acc)
     (if (and (not (empty? acc))
              (equal? (second acc) '...))
         `(pa ,x ,(third acc))
         `(pl ,x ,acc)))
   '() stx))

(check-equal? (parse-ellipses-pattern '(1 2 3 d ... 5 f ... 7 8))
              '(pl 1 (pl 2 (pl 3 (pa d (pl 5 (pa f (pl 7 (pl 8 ())))))))))
#|
algo attempt 2:

(pa new-list-pat-var rest-pat)
so for pa pattern-case, if we want greedy behavior, lets try starting at
the end. take the 0-tail of the template, and match it against rest-pat.
if it works, then bind the initial segment minus the 0-tail (so whole list)
to the new-list-pat-var. otherwise, take the 1-tail, and continue as
such, taking the n-tail, until n-tail = whole template list, in which case
return no match.

|#



; sketch 1: pm is pattern match fn
#; (match* (pat tem)
     [(`(pl ,pat-a ,rest-pat-b)
       `(*cons ,tem-a ,rest-tem-b))
      ; check if no-match
      (hash-union (pm pat-a tem-a) (pm rest-pat-b rest-tem-b))]
     [(`(pa ,init-seg-pat ,new-pat-var-a)
       template)
      (define (try init-seg-tem end-seg-tem)
        (match init-seg-tem
          ['() 'no-match] ;or is this okay sometimes??
          [_ (match (pm init-seg-pat init-seg-tem)
               ['no-match (try (drop1 init-seg-tem) (append (last init-seg-tem) end-seg))]
               [new-env (hash-set new-env new-pat-var-a end-seg-tem)])]))
      (try template '())])


;given:
#;'(pat-a pat-b pat-c ... pat-d pat-e ...)
#;(match whatever
    [`(,(and xs (not '...) ... '...) )])

; REMEMBER still need to do pre-parser!!

; lets try to integrate this in basic pm code:
(require racket/hash)
(define (pattern-match types c-env arg pat)
  (println "pm")
  (println arg)
  (println pat)
  (define constructor-id?
    (curry hash-has-key? types))
  (cond [(and (or (number? pat) (constructor-id? pat))
              (equal? arg pat))
         c-env]
        [(and (not (constructor-id? pat))
              (symbol? pat))
         (hash-set c-env pat arg)]
        [(and (list? pat)
              (not (empty? pat))
              (equal? (first pat) 'pl)
              (list? arg)
              (not (empty? arg)))
         #;(println "pl case")
         (if (or (equal? 'no-match (pattern-match types c-env (first arg) (second pat) ))
                 (equal? 'no-match (pattern-match types c-env (rest arg) (third pat) )))
             'no-match
             (hash-union (pattern-match types c-env (first arg) (second pat) )
                         (pattern-match types c-env (rest arg)(third pat) )))
         ]
        [(and (list? pat)
              (not (empty? pat))
              (equal? (first pat) 'pa)
              (list? arg)
              (or (not (empty? arg)) (empty? (third pat))))
         #;(println "pa case")
         (define (try init-seg-tem end-seg-tem)
           #;(displayln "try")
           #;(displayln end-seg-tem)
           #;(displayln (third pat))
           (match init-seg-tem
             ['() (println "recursed to empty case")
                  (if (equal? 'no-match (pattern-match types c-env end-seg-tem (third pat)))
                      'no-match
                      (hash-set (pattern-match types c-env end-seg-tem (third pat)) (second pat) init-seg-tem))]
             [_ (match (pattern-match types c-env end-seg-tem (third pat))
                  ['no-match (try (drop-right init-seg-tem 1)
                                  (cons (last init-seg-tem) end-seg-tem))]
                  [new-env
                   #;(println "newenv case")
                   (define res (foldl (λ (x acc)
                                        (if (equal? 'no-match acc)
                                            'no-match
                                            (if (equal? 'no-match (pattern-match types (hash) x (second pat)))
                                                'no-match
                                                (hash-union
                                                 acc
                                                 (make-hash (hash-map (pattern-match types (hash) x (second pat))
                                                                      (λ (k v) (cons k (list v)))))
                                                 #:combine (λ (a b) (append a b))))))
                                      (hash)
                                      init-seg-tem))
                   (if (equal? res 'no-match)
                       (try (drop-right init-seg-tem 1)
                            (cons (last init-seg-tem) end-seg-tem))
                       (hash-union new-env res))
                   #;(hash-set new-env (second pat) init-seg-tem)])]))
         (try arg '())]
        [(and (list? pat)
              (list? arg)
              (equal? (length pat) (length arg)))
         #;(println "plain list case")
         (foldl (λ (arg pat env)
                  (pattern-match types env arg pat))
                c-env arg pat)]
        [else 'no-match]))



(check-equal? (pattern-match (hash) (hash) '(1 3 4) '(pl a (3 4)))
              #hash((a . 1)))

(check-equal? (pattern-match (hash) (hash) '(1 3 4) '(pl 1 (3 4)))
              #hash())

(check-equal? (pattern-match (hash) (hash) '(1 3 4) '(pl 1 (pl 3 (pl 4 ()))))
              #hash())


(check-equal? (pattern-match (hash) (hash)
                             '(1 2)
                             '(pa a (2)))
              #hash((a . (1))))


(check-equal? (pattern-match (hash) (hash)
                             '(1 2)
                             '(pa a (pl 2 ())))
              #hash((a . (1))))

(check-equal? (pattern-match (hash) (hash) '((1 1) (1 2) (4 5) 3 4) '(pa (a b) (3 4)))
              #hash((a . (1 1 4)) (b . (1 2 5))))

(check-equal? (pattern-match (hash) (hash)
                             '(1 2 3 4 5 6 7 8)
                             (parse-ellipses-pattern '(1 2 3 d ...)))
              '#hash((d . (4 5 6 7 8))))


(check-equal? (pattern-match (hash) (hash)
                             '(1 2 3 4 5)
                             (parse-ellipses-pattern '(1 2 3 d ... 5)))
              '#hash((d . (4))))


(check-equal? (pattern-match (hash) (hash)
                             '(1 2 3 4 5 6)
                             (parse-ellipses-pattern '(1 2 3 d ... 5 f ... 6)))
              #hash((f . ()) (d . (4))))

(check-equal? (pattern-match (hash) (hash)
                             '(1 2 3 4 5 6 7)
                             (parse-ellipses-pattern '(1 2 3 d ... 5 f ... 7)))
              #hash((f . (6)) (d . (4))))


(check-equal? (pattern-match (hash) (hash)
                             '(1 2 3 4 4 5 6 6 6 7 8)
                             (parse-ellipses-pattern '(1 2 3 d ... 5 f ... 7 8)))
              #hash((f . (6 6 6)) (d . (4 4))))


(check-equal? (pattern-match (hash) (hash)
                             '(1 2 3 4 5)
                             (parse-ellipses-pattern '(a ... 2 b ... 5)))
              #hash((a . (1)) (b . (3 4))))


(check-equal? (pattern-match (hash) (hash)
                             '(1 2 3 4 5)
                             (parse-ellipses-pattern '(1 a ... 2 b ... 5)))
              #hash((a . ()) (b . (3 4))))

(check-equal? (pattern-match (hash) (hash)
                             '(1)
                             '(pl f ()))
              #hash((f . 1)))

(check-equal? (pattern-match (hash) (hash)
                             '(1)
                             (parse-ellipses-pattern '(1 f ...)))
              #hash((f . ())))

(check-equal? (pattern-match (hash) (hash)
                             '(1 2 3 4 5)
                             (parse-ellipses-pattern '(1 2 3 d ... 5 f ...)))
              #hash((f . ()) (d . (4))))

#|
side note: for explaining pattern matching; the need to distiguish
datums/literals from pattern variables: take the analogy of expanding
a fn in source code. looking for a fn call, the fn name is a literal
and the parameters are captured as pattern variable (modulo evaluation
strategy of course; lead into discussion of macros)
|#

