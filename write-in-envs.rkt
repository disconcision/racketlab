#lang racket


(define p1
  '(◇
    (p/
     #hash((sort . expr))
     (λ ((p/ #hash((sort . pat)) ⊙)) (p/ #hash((sort . expr) (▹ . ▹)) ⊙)))))


(define p2
  '(◇
    (p/
     #hash((sort . expr))
     (λ ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name))))
       (p/ #hash((sort . expr)) ⊙)))))

(define p3
  '(◇
    (p/
     #hash((sort . expr))
     (app
      (p/
       #hash((sort . expr)
             (▹ . ▹))
       ⊙)
      (p/
       #hash((sort . expr))
       ⊙)))))


(define (write-in-envs env stx)
  (define W (curry write-in-envs env))
  (match stx
    [`(◇ (p/ ,top-hash ,prog))
     `(◇ (p/ ,(hash-set top-hash 'in-scope env) ,prog))]
    [`(λ ((p/ ,a (var (p/ ,b ,id))))
        (p/ ,body-anns ,body))
     (define new-env `(,id ,@env))
     `(λ ((p/ ,a (var (p/ ,b ,id))))
        (p/ ,(hash-set body-anns 'in-scope new-env) ,(W body)))]


    [`(app (p/
            ,a
            ,f-expr)
           (p/
            ,b
            ,a-expr))
     `(app (p/
            ,(hash-set a)
            ,f-expr)
           (p/
            ,b
            ,a-expr))]))


