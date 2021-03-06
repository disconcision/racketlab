#lang racket

; andrew blinn 2018

(provide step-choice)


; tests commented out to supress printing
(module+ test
  (require rackunit)
  (check-equal? (step-choice 0)
                '(0))
  (check-equal? (step-choice '(list))
                '(()))
  (check-equal? (step-choice '(list 1 2 3))
                '((1 2 3)))
  (check-equal? (step-choice '(list 1 (list 2 3) 4))
                '((1 (2 3) 4)))

  (check-equal? (step-choice '(next))
                '())

  (check-equal? (step-choice '(-<))
                '())
  (check-equal? (step-choice '(-< 1))
                '(1))
  (check-equal? (step-choice '(-< 1 2 3))
                '(1 2 3))

  (check-equal? (step-choice '(list 0 (-< 1 2 3)))
                '((0 1) (0 2) (0 3)))
  (check-equal? (step-choice '(list (-< 1 2) (-< 3 4)))
                '((1 3) (1 4) (2 3) (2 4)))
  (check-equal? (step-choice '(-< (list 1 2) 3))
                '((1 2) 3))
  (check-equal? (step-choice '(-< (list (-< 1 2)) 3))
                '((1) (2) 3))

  (check-equal? (step-choice '(list (next)))
                '())
  (check-equal? (step-choice '(list (-<)))
                '())

  (check-equal? (step-choice '(-< (-< (next) 2) 3))
                '(2 3))
  (check-equal? (step-choice '(-< (list (-< 1 2) (next)) 3))
                '(3)))


(define (expr!? stx)
  ; errors if stx is syntacically incorrect
  (match stx
    [(? number?) stx]
    [`(next) `(next)]
    [`(list ,xs ...)
     `(list ,@(map expr!? xs))]
    [`(-< ,xs ...)
     `(-< ,@(map expr!? xs))]
    [_ (error (~a `(bad syntax: ,stx)))]))


(define (value? stx)
  ; checks if stx represents a value
  (match stx
    [(? number?) #t]
    ['() #t]
    [`(list ,xs ...) #f]
    [`(next) #f]
    [`(-< ,xs ...) #f]
    [(? list?) (andmap value? stx)]))


(define-values (peek pop push)
  ; stack implementation
  (values first rest cons))


(define (step stx stack)
  ; small-steps state where ▹ indicates current redex
  
  (define new-stx
    (match stx
      [(or (⋱ c `(▹ (-<)))
           (⋱ c `(▹ (next))))
       (if (empty? stack)
           `done
           (peek stack))]
      [(⋱ c `(▹ (-< ,a)))
       (⋱ c `(▹ ,a))]
      [(⋱ c `(▹ (-< ,a ,as ...)))
       (⋱ c `(▹ (-< ,a)))]
      ; if all args to list are values, return literal
      [(⋱ c `(▹ (list ,(? value? xs) ...)))
       (⋱ c `(▹ ,xs))]
      ; otherwise, step into the list,
      [(⋱ c `(▹ (list ,x ,xs ...)))
       (⋱ c `(list (▹ ,x) ,@xs))]
      ; evaluating the arguments left-to-right,
      [(⋱ c `(list ,xs ... (▹ ,(? value? x)) ,y ,ys ...))
       (⋱ c `(list ,@xs ,x (▹ ,y) ,@ys))]
      ; stepping out after the last arg is evaluated
      [(⋱ c `(list ,xs ... (▹ ,(? value? x))))
       (⋱ c `(▹ (list ,@xs ,x)))]))

  (define new-stack
    (match stx
      [(or (⋱ c `(▹ (-<)))
           (⋱ c `(▹ (next))))
       (if (empty? stack)
           '()
           (begin
             (println `(pop-stack))
             (pop stack)))]
      [(⋱ c `(▹ (-< ,a ,as ..1)))
       ; alternate display: (continuation (▹ redex))
       #;(println `(push-stack: ,(⋱ c `(▹ (-< ,@as)))))
       (println `(push-stack: ,(c '_) (-< ,@as)))
       (push (⋱ c `(▹ (-< ,@as))) stack)]
      [_ stack]))

  (values new-stx new-stack))


(define (step-until-value stx stack)
  ; steps stx/stack until stx is a value and the stack is empty
  (unless ((match-lambda? `done) stx)
    (println stx))
  (match stx
    ; if stx is done, the stack is empty
    [`done '()]
    ; if stx is fully evaluated, check the stack
    [`(▹ ,(? value? v))
     (match stack
       ['() `(,v)]
       [`(,x ,xs ...)
        (println `(pop-stack-auto))
        `(,v ,@(step-until-value x xs))])]
    ; otherwise, keep evaluating
    [_ (call-with-values (thunk (step stx stack))
                         step-until-value)]))


(define (step-choice stx)
  ; returns a list of all results from evaluating stx
  (step-until-value `(▹ ,(expr!? stx)) '()))



; ------------------------------

; this is library code to implement context patterns
; dumped here to make this standalone
; (see containment-patterns on my github)

(define-match-expander ⋱
  ; containment pattern (returns first match)
  (λ (stx)
    (syntax-case stx ()
      [(⋱ <internal-pat>)
       #'(⋱ _ <internal-pat>)]
      [(⋱ context-id <internal-pat>)
       #'(app
          (curry first-containment (match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat>)))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱ context-id internal-template)
       #'(context-id internal-template)])))


(define-syntax-rule (match-lambda? <pat>)
  ; converts a pattern into a predicate
  (match-lambda [<pat> #t] [_ #f]))


(define (multi-split ls lengths)
  ; splits list ls into segments of lengths lengths
  (unless (equal? (length ls)
                  (apply + lengths))
    (error "length of list doesn't partition"))
  (define-values (actual extra)
    (for/fold ([acc '()]
               [ls ls])
              ([l lengths])
      (define-values (this others)
        (split-at ls l))
      (values (cons this acc) others)))
  (reverse actual))


(define (multi-containment match? xs (until? (λ (x) #f)))
  ; this returns a list of two elements
  ; the first element is the multi-holed context as a fn
  ; the second is a list of the contents of those holes
  (cond
    [(match? xs)
     (list (λ (x) x) `(,xs))]
    [(or (not (list? xs)) (until? xs))
     (list (λ () xs) `())]
    [else
     (define subpairs
       (for/list ([x xs])
         (multi-containment match? x until?)))
     (define subcontexts
       (map first subpairs))
     (define submatches
       (apply append (map second subpairs)))
     (define subcontext-arities
       (map procedure-arity subcontexts))
     (define (context-candidate . args)
       (for/list ([subfn subcontexts]
                  [arg-list (multi-split args
                                         subcontext-arities)])
         (apply subfn arg-list)))
     (define new-context
       (procedure-reduce-arity context-candidate
                               (apply + subcontext-arities)))
     (list new-context
           submatches)]))


(define (first-containment match? xs (until? (λ (x) #f)))
  ; this returns a list of two elements
  ; the first element is a one-holed context as a fn
  ; the second is a one-element list of the content of that hole
  ; this currently is just a gloss for mult-containment
  ; it could be implemented more efficiently separately
  (match-define (list context matches)
    (multi-containment match? xs until?))
  (if (empty? matches)
      (list context matches)
      (list (λ (x) (apply context x (rest matches)))
            (list (first matches)))))
