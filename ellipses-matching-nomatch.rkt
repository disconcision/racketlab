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