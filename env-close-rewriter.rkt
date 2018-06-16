#lang racket





#; (dat d)
#; (var id)
#; (fun (id) expr)
#; (app expr expr)
#; (set id expr)


(module+ test
  (require rackunit)
  (check-equal? 0 0 ))



(define (E expr parents)
  (define (reconstruct expr parents)
    ((apply compose (reverse parents)) expr))
  (println (reconstruct expr parents))
  (define (get-parent-env) 0)
  (define (lookup id) 0)
  (define evald-expr
    (match expr
      [`(dat ,d)
       `(dat ,d)]
      
      [`(fun (,id) ,body)      
       `(CL (,id ?) ,body)]
      ; should i just capture all of parents in closure? but then how would set work...
      [`(CALL (CL (,id ?) ,body) ,arg)
       (first (E `(Ei ((,id ,arg)) ,body) parents))]
      
      [`(app ,f-expr ,a-expr)
       (match-let ([`(,new-expr ,new-parents) (E f-expr `(,(λ (x) `(app ,x ,a-expr)) ,@parents))])
         (match-let ([`(,new-expr-1 ,new-parents-1) (E a-expr `(,(λ (x) `(app ,new-expr ,x)),@parents))])
           (first (E `(CALL ,new-expr ,new-expr-1) parents))))] ;todo: reconstruct parents with possible changes from set
    
      [`(Ei ,pairs (var ,id)) `(Ei ,pairs ,(second (assoc id pairs)))]
      [`(var ,id) (lookup id)]
      [`(set ,id ,expr) 'VOID])) ; set effects parents; can we make this work?
  `(,evald-expr ,parents))


(first (E '(app (fun (x) (var x)) (dat 5)) `(,identity)))