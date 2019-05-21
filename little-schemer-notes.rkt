#lang racket

(define-syntax-rule
  (prototype
      <fn> [<in> ... => <out>] ...)
  (module+ test
    (require rackunit)
    (check-equal?
     (<fn> <in> ...) <out>) ...))

(require rackunit)
(define-syntax-rule
  (λ+ (<p> ...)
    [<in> ... => <out>] ...
    <body>)
  (let ([f (λ (<p> ...) <body>)])
    0 (check-equal?
       (f <in> ...) <out>) ...))

(define zero '())
(define one '(S))
(define two '(S S))
(define three '(S S S))
(define add1
  (λ+ (n)
    ['() => '(S)]
    ['(S) => '(S S)]
    ['(S S) => '(S S S)]
    (cons 'S n)))
(define sub1
  (λ+ (n)
    ['() => '()]
    ['(S) => '()]
    ['(S S) => '(S)]
    ['(S S S) => '(S S)]
    (cond
      [(null? n) '()]
      [else (cdr n)])))

(define nth
  (λ (n xs)
    (cond
      [(null? xs) '()]
      [else (cond
              [(null? n) (car xs)]
              [else (nth (cdr n)
                         (cdr xs))])])))
(prototype nth
  ['() '() => '()]
  ['() '(A) => 'A ]
  ['() '(A B) => 'A]
  ['(S) '(A B) => 'B]
  ['(S) '(A B C) => 'B]
  ['(S S) '(A B C) => 'C])


(define nth2
  (λ (n xs)
    (cond
      [(null? n) (car xs)]
      [else (cond
              [(null? xs) '()]
              [else (nth2 (cdr n)
                          (cdr xs))])])))