#lang racket
(require racket/hash)
(define (pattern-match types c-env arg pat)
  (define (bind x f)
    (if (equal? 'no-match x)
        'no-match
        (f x)))
  (define (append-hashes h1 h2)
    (hash-union
     h1
     (make-hash (hash-map h2 (λ (k v) (cons k (list v)))))
     #:combine (λ (a b) (append a b))))
  (define (accumulate-over-seg x acc)
    (bind acc
          (λ (_)
            (bind (pattern-match types (hash) x (second pat))
                  (curry append-hashes acc)))))
  (define constructor-id?
    (curry hash-has-key? types))
  (define Pm (curry pattern-match types c-env))
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
           (bind (Pm (first arg) (second pat))
                 (λ (env-1)
                   (bind (Pm (rest arg) (third pat))
                         (curry hash-union env-1))))]
          [(and (list? pat)
                (not (empty? pat))
                (equal? (first pat) 'pa)
                (list? arg)
                (or (not (empty? arg)) (empty? (third pat))))
           (define (try arg-seg-init arg-seg-tail)
             (match arg-seg-init
               ['() (bind (Pm arg-seg-tail (third pat))
                          (curryr hash-set (second pat) arg-seg-init))]
               [_ (match (Pm arg-seg-tail (third pat))
                    ['no-match (try (drop-right arg-seg-init 1)
                                    (cons (last arg-seg-init) arg-seg-tail))]
                    [new-env
                     (match (foldl accumulate-over-seg (hash) arg-seg-init)
                       ['no-match (try (drop-right arg-seg-init 1)
                                       (cons (last arg-seg-init) arg-seg-tail))]
                       [old-env (hash-union new-env old-env)])])]))
           (try arg '())]
          [(and (list? pat)
                (list? arg)
                (equal? (length pat) (length arg)))
           (foldl (λ (arg pat env)
                    (pattern-match types env arg pat))
                  c-env arg pat)]
          [else 'no-match]))




; NOTES


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