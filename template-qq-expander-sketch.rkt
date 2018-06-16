#lang racket

(require rackunit)
(require (rename-in racket (quasiquote qq-original)))
(define-syntax quasiquote
  (syntax-rules (...)))

(begin-for-syntax
  (define ((new-qq env) template)
    0
    ))
; figure out how to use ... literal in syntax-rules

; parse body replacing x ... with (ooo x)


#;
(check-equal? '(new-qq (a (1 2 3)) (,a ...))
              '(1 2 3))
#;
(check-equal? '(new-qq (a (1 2 3)) (0 ,a ... 4))
              '(0 1 2 3 4))
#;
(check-equal? '(new-qq (a (1 2 3)) (0 ,a ... 4))
              '(0 1 2 3 4))


(check-equal? '(new-qq-rr (,a ...))
              '(append a)
              #;'a)

(check-equal? '(new-qq-rr (0 ,a ... 4))
              #; (new-qq-rr (0 (ooo a) 4))
              '(append (0) a (4)))

(check-equal? '(new-qq-rr (,a 0 ,b ... 4 ,c))
              '(append (,a 0) b (4 ,c)))

(check-equal? '(new-qq-rr (,a ... ,b ...))
              '(append a b))

(check-equal? '(new-qq-rr ((,a ,b) ...))

              '(map (λ (x y) `(,x ,y)) a b)
              #;'(map list a b))

(check-equal? '(new-qq-rr ((,a 0) ...))
              #; '(map list a (make-list 0 (length a)))
              '(map (λ (x) `(,x 0)) a))

(check-equal? '(new-qq-rr ((,a ...) ...))
              '(map (λ (x) (append x)) a))

(check-equal? '(new-qq-rr '((0 ,a ...) ...)) ; say a is ((1 2) (3) (4 5))
              '(map (λ (x) (append '(0) x)) a)) ; ((x 1 2) (x 3) (x 4 5))