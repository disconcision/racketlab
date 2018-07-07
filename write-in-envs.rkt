#lang racket


(define p1
  '(◇
    (p/
     #hash((sort . expr))
     (λ ((p/ #hash((sort . pat)) ⊙)) (p/ #hash((sort . expr) (▹ . ▹)) ⊙)))))


(define p2
  '(◇
    (p/ #hash((sort . expr))
        (λ ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name))))
          (p/ #hash((sort . expr))
              (λ ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name2))))
                (p/ #hash((sort . expr))
                    (app (p/ #hash((sort . expr) (▹ . ▹)) ⊙)
                         (p/ #hash((sort . expr)) ⊙)))))))))


(module+ test
  (require rackunit)
  (check-equal?
   (write-in-envs p2 )
   '(◇ (p/
        #hash((in-scope . ()) (sort . expr))
        (λ ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name))))
          (p/ #hash((in-scope . (name)) (sort . expr))
              (λ ((p/ #hash((sort . pat) (▹ . ▹)) (var (p/ #hash((sort . char)) name2))))
                (p/ #hash((in-scope . (name2 name)) (sort . expr))
                    (app (p/ #hash((in-scope . (name2 name)) (sort . expr) (▹ . ▹)) ⊙)
                         (p/ #hash((in-scope . (name2 name)) (sort . expr)) ⊙))))))))))




(define (write-in-envs stx)
  (define W (curry write-in-envs))
  (match stx
    
    [`(◇ (p/ ,anns ,prog))
     `(◇ ,(W `(p/ ,(hash-set anns 'in-scope '()) ,prog)))]

    [`(p/ ,anns ⊙)
     `(p/ ,anns ⊙)]
    
    [`(p/ ,(and top-hash (hash-table ('in-scope env)))
          (λ ((p/ ,a (var (p/ ,b ,id))))
            (p/ ,body-anns
                ,body)))
     `(p/ ,top-hash
          (λ ((p/ ,a (var (p/ ,b ,id))))
            ,(W `(p/ ,(hash-set body-anns 'in-scope `(,id ,@env))
                     ,body))))]
    
    [`(p/ ,(and top-hash (hash-table ('in-scope env)))
          (app (p/ ,a ,f-expr)
               (p/ ,b ,a-expr)))
     `(p/ ,top-hash
          (app ,(W `(p/ ,(hash-set a 'in-scope env) ,f-expr))
               ,(W `(p/ ,(hash-set b 'in-scope env) ,a-expr))))]))


