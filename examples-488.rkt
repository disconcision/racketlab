#lang racket
'(begin
                     
   (define true
     (λ (x)
       (λ (y) (x 666))))
                     
   (define false
     (λ (x)
       (λ (y) (y 666))))

   (define empty
     (λ (msg)
       true))

   (define (empty? b)
     (b true))

   (define (box val)
     (λ (msg)
       (if msg
           false
           val)))

   (define (unbox b)
     (b false))

   (define (pair a b)
     (λ (msg)
       (if msg a b)))

   (define (aft p)
     (p false))

   (define (fore p)
     (p true))

   (define (cons e ls)
     (box (pair e ls)))

   (define (first ls)
     (fore (unbox ls)))

   (define (rest ls)
     (aft (unbox ls)))

   (define (compose f g)
     (λ (x) (f (g x))))

   (define (identity x) x)

   (define second
     (compose first rest))

   (define third
     (compose second rest))        

   ; right now everything is autocurried
   #; (define (curry f a)
        (λ (x) (f a x)))

   (define (fold f init ls)
     (if (empty? ls)
         init
         (f (first ls)
            (fold f
                  init
                  (rest ls)))))

   (define (append a b)
     (fold cons b a))

   (define (map f ls)
     (fold (λ (x ys)
             (cons (f x) ys))
           empty ls))

   (define (filter f ls)
     (fold (λ (x ys)
             (if (f x)
                 (cons x ys)
                 ys))
           empty ls))
                     
   ; introducing zero, increment
   (define (length ls)
     (fold (λ (a bs)
             (+ 1 bs))
           0 ls))

   ; todo: try implementing foldl with foldr                    
   (define (foldl f accum ls)
     (if (empty? ls)
         accum
         (foldl f
                (f (first ls) accum)
                (rest ls))))

   (define (reverse ls)
     (foldl cons
            empty ls))

   (define last
     (compose first
              reverse))                   
                     
   ; introducing decrement
   (define (list-ref ls i)
     (if (< i 1)
         (first ls)
         (list-ref (rest ls)
                   (+ i -1))))

   (define (not a)
     (if a
         false
         true))

   (define (or a b)
     (if a
         true
         (if b
             true
             false)))

   (define (and a b)
     (if a
         (if b
             true
             false)
         false))

   (define (>= a b)
     (not (< a b)))

   ; equal? for integers
   (define (=? a b)
     (and (>= a b)
          (< a (+ 1 b))))

   (define (<= a b)
     (or (< a b)
         (=? a b)))

   (define (> a b)
     (not (<= a b)))

   ; not very efficient (append, double filtering)
   (define (quicksort ls)
     (if (< (length ls) 2)
         ls
         (begin
           (define pivot
             (first ls))
           (define under
             (filter (λ (x)
                       (< x pivot))
                     ls))
           (define over
             (rest (filter (λ (x)
                             (>= x pivot))
                           ls)))
           (append (quicksort under)
                   (append (cons pivot empty)
                           (quicksort over))))))

   ; todo:
   #;(define (list-equal? a b)); needs types?
   #;(define member? 0); needs equality
   #;(define (assoc v ls)); needs equality
   ; numerical functions: add1 sub1 - %
   ; boolean forms in R* (short circuiting)
   ; boolean combinators: negate conjoin disjoin
                     

   ; test tests (yo dawg)

   (define list-A
     (cons 11
           (cons 22
                 (cons 33
                       (cons 44
                             empty)))))

   (define list-B
     (cons 55
           (cons 66
                 empty)))

   (define list-unsorted
     (cons 55
           (cons 22
                 (cons 44
                       (cons 11
                             (cons 33 empty))))))

   ; this will output the numeric label of the first
   ; failing test, or 0 if all tests succeed
                     
   (check =?
          ((1  (first list-A) 11)
           (2  (first (rest list-A)) 22)
           (3  (second list-A) 22)
           (4  (third list-A) 33)
           (5  (first (reverse list-A)) 44)
           (6  (last list-A) 44)

           (7  (length list-A) 4)
           (8  (second (map (λ (x) (+ x 4)) list-A)) 26)
           (9  (third (reverse (append list-A list-B))) 44)
           (10 (list-ref list-A 2) 33)
           (11 (length (filter (λ (x) (> x 22)) list-unsorted)) 3)
           (12 (first (filter (λ (x) (> 22 x)) list-unsorted)) 11)

           (13 (if (empty? empty) 1 0) 1)
           (14 (if (empty? (cons 1 empty)) 1 0) 0)

           ; these three don't actually test much...
           (15 (if (=? 3 3) 1 0) 1)
           (16 (if (=? 4 3) 1 0) 0)
           (17 (if (=? 3 4) 1 0) 0)

           (18 (if (>= 4 4) 1 0) 1)
           (19 (if (>= 4 3) 1 0) 1)
           (20 (if (>= 3 4) 1 0) 0)

           (21 (if (<= 4 4) 1 0) 1)
           (22 (if (<= 4 3) 1 0) 0)
           (23 (if (<= 3 4) 1 0) 1)

           (24 (if (> 4 4) 1 0) 0)
           (25 (if (> 4 3) 1 0) 1)
           (26 (if (> 3 4) 1 0) 0)
                               
           (27 (first (quicksort list-unsorted)) 11)
           (28 (second (quicksort list-unsorted)) 22)
           (29 (third (quicksort list-unsorted)) 33)
                                
           (666 0 1)))
   )