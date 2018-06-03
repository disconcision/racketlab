#lang racket

(require rackunit)

; match ellipses are greedy:

(check-equal?
 (match '(1 1 1 1 1 2 1 1 1 1 2)
   [`(,a ... 2 ,b ...)
    a])
 '(1 1 1 1 1 2 1 1 1 1))

(check-equal?
 (match '(1 1 1 1 1 2 1 1 1 1 3)
   [`(,a ... 2 ,b ...)
    a])
 '(1 1 1 1 1))

(check-equal?
 (match '(2 1 1 1 1 2 1 1 1 1 2)
   [`(,a ... 2 ,b ...)
    a])
 '(2 1 1 1 1 2 1 1 1 1))

(check-equal?
 (match '(1 1 1 1 1)
   [`(,a ... 1 ,b ...)
    a])
 '(1 1 1 1))




#; `(,a ... 2 ,b ...)
#; `(p-append `(,a ... 2) `(,b ...))
#; `(p-append `(,a ... 2) b) ; where b is list
#; `(p-append a '(2) b) ; where a, b are lists
#; `(p-append (p-append a '(2)) b) ; where a, b are lists
; or:
#; `(p-append a (p-append '(2) b)) ; where a, b are lists
; the later suggests:
#|
bind a to list contining first element, then try to match 2nd arg (rest of pattern) to
rest of list. if suceed, call binding of a last-success, and rest of list last-rest
then try to bind a to list containing first two elements, etc, until the end of the list
we're matching against. then we return the last-success binding of a, unioned with
the bindings from applying the rest of the pattern to last-rest

|#

; what do we need to forbid?
#; (p-append a b) ; for lists a, b (what if p not a list pat?)
#; (p-append a (p-append b whatever)) ; for lists a, b


#|
first, lets rewrite ...-containing variable-length list patterns:
pc = pcons: operates like cons  (first elem is elem, second elem is list)
pa = pappend: 
|#
#; `(A B C ,d ... E ,f ... G H) 
#; `(pl A (pl B (pl C (pa ,d (pl E (pa ,f (pl G (H))))))))
; list* style alernative parsing:
#; `(pl* A B C (pa d (pl* E (pa f (pl* G H)))))

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

; ACHTUALLY: whoops, below assumes i'm parsing from the right, not the left..
; i.e.:
#; `(p-append (p-append a '(2)) b)
#; `(pl (pl (pa (pl (pa (pl (pl A (B)) C) ,d) E) ,f) G) H)
#; `(*pl (pa (*pl (pa (*pl A B C) ,d) E) ,f) G H)
; maybe should just reverse the list to start... lesser of evils?

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


; REMEMBER still need to do pre-parser!!

; lets try to integrate this in basic pm code:
(require racket/hash)
(define (pattern-match types c-env arg pat)
  (define constructor-id?
    (curry hash-has-key? types))
  (cond [(and (constructor-id? pat)
              (equal? arg pat))
         c-env]
        [(and (not (constructor-id? pat))
              (symbol? pat))
         (hash-set c-env pat arg)]
        [(and (list? pat)
              (equal? (first pat) 'pl))
         (hash-union (pattern-match (second pat) (first arg))
                     (pattern-match (third pat) (rest arg)))]
        [(and (list? pat)
              (equal? (first pat) 'pa))
         (define (try init-seg-tem end-seg-tem)
           (match init-seg-tem
             ['() (println "recursed to empty case") 'no-match] ;or is this okay sometimes??
             [_ (match (pattern-match (second pat) init-seg-tem)
                  ['no-match (try (drop init-seg-tem (+ (length init-seg-tem) -1))
                                  (append (last init-seg-tem) end-seg-tem))]
                  [new-env (hash-set new-env (third pat) end-seg-tem)])]))
         (try arg '())]
        [(list? pat)
         (foldl (λ (arg pat env)
                  (pattern-match types env arg pat))
                c-env arg pat)]
        [else 'no-match]))


#|
side note: for explaining pattern matching; the need to distiguish
datums/literals from pattern variables: take the analogy of expanding
a fn in source code. looking for a fn call, the fn name is a literal
and the parameters are captured as pattern variable (modulo evaluation
strategy of course; lead into discussion of macros)
|#

