#lang racket


(provide f/match phash phash-plus)


; still need to do ellipses patterns

(require (for-syntax racket/match)
         racket/hash)

(define-for-syntax (rewrite-p/ stx)
  (match stx
    [(list anns ... and-pat '... / a)
     (list 'quasiquote (list 'p/ (list 'unquote `(phash-plus ,@anns ,and-pat)) (list 'unquote (rewrite-p/ a))))]
    [`(,anns ... / ,a)
     (list 'quasiquote (list 'p/ (list 'unquote `(phash ,@anns)) (list 'unquote (rewrite-p/ a))))]    
    [(? list?) (map rewrite-p/ stx)]
    [_ stx]))

(define-for-syntax (rewrite-pairs stx)
  (match stx
    [`(,pat ,xs ... ,tem)
     `(,(rewrite-p/ pat) ,@xs ,(rewrite-p/ tem))]
    [_ stx]))

(define-syntax (f/match stx)
  (syntax-case stx (/)
    [(g/match source pairs ...)
     (let ([new-pairs (map rewrite-pairs (syntax->datum #'(pairs ...)))])
       (with-syntax ([(newest-pairs ...) (datum->syntax stx new-pairs)])
         #'(match source newest-pairs ...)))]))


(define-match-expander phash
  (λ (stx)
    (syntax-case stx ()
      [(phash anns ...)
       (let ([new-anns
              (for/list ([ann (syntax->datum #'(anns ...))])
                (if (symbol? ann)
                    `(,(list 'quote ann) ,ann)
                    ann))])
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash-table newest-anns ...)))]))
  (λ (stx)
    (syntax-case stx ()
      [(phash anns ...)
       (let ([new-anns
              (apply append
                     (for/list ([ann (syntax->datum #'(anns ...))])
                       (if (symbol? ann)
                           `(,(list 'quote ann) ,ann)
                           ann)))])
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash newest-anns ...)))])))


(define-match-expander phash-plus
  (λ (stx)
    (syntax-case stx ()
      [(phash-plus anns ... rest-pat)
       (let ([new-anns
              (for/list ([ann (syntax->datum #'(anns ...))])
                (if (symbol? ann)
                    `(,(list 'quote ann) ,ann)
                    ann))])
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash-table newest-anns ... rest-pat (... ...))))]))
  (λ (stx)
    (syntax-case stx ()
      [(phash-plus anns ... rest-pat)
       (let ([new-anns
              (apply append
                     (for/list ([ann (syntax->datum #'(anns ...))])
                       (if (symbol? ann)
                           `(,(list 'quote ann) ,ann)
                           ann)))])
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash-union (for/fold ([acc #hash()])
                                   ([r rest-pat])
                           (match r
                             [`(,k ,v) (hash-set acc k v)]))
                         newest-anns ...)))])))

(module+ test
  (require rackunit)
  (check-equal? (match #hash((a . 70))
                  [(phash a) a])
                70)
  (check-equal? (match #hash((a . 70) (b . 80))
                  [(phash-plus b rd) rd])
                '((a 70)))

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 2))
                  [( > a ... / (b / 2))
                   a])
                '((sort expr)))

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 666))
                  [( > a ... / (b / 666))
                   (a ... / (b / 667))])
                '(p/
                  #hash((sort . expr))
                  (p/ #hash((b . 8)) 667)))

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 2))
                  [( > (sort 'expr) / (b / 2))
                   b])
                8)

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 2))
                  [(> (sort 'expr) / (b / 2))
                   (> (sort 'pat) / (b / 2))])
                '(p/
                  #hash((sort . pat) (> . >))
                  (p/ #hash((b . 8)) 2)))
  )














