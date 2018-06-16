#lang racket

(require rackunit)
(require racket/hash)

#|

2018.06.15

This file started out as an exercise in implementing
racket/match style ellipses patterns. it since grew
into a more comprehensive edit-time oriented rewriting
system, still following the lead of racket/match.

Said implemention has since been relabelled fructerm;
see that github repo for more information.

|#


#;(define env (destructure types #hash() source pattern))
#;(if (equal? 'no-match env)
      (runtime-match types other-clauses source)
      (restructure types env template))


; gonna try to reproduce racket/match ellipses matching
; so i can use ellipses-based list patterns in my run-time matcher

; recall that match ellipses are greedy:

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


; to simply the problem i'm considering a rewriting stage:

; i define a new pattern form;
#; (p... <init-pat> <rest-pat>)

#; `(,a ... 2 ,b ...)
; would be equivalent to:
#; `(p... `(,a ... 2) `(,b ...))
#; `(p... `(,a ... 2) b) ; where b is list
; parsing associativity options:
#; `(p... (p... a '(2)) b) ; where a, b are lists
; or:
#; `(p... a (p... '(2) b)) ; where a, b are lists
; the later suggests:

#|
bind a to list contining first element, then try to match 2nd arg (rest of pattern) to
rest of list. if suceed, call binding of a last-success, and rest of list last-rest
then try to bind a to list containing first two elements, etc, until the end of the list
we're matching against. then we return the last-success binding of a, unioned with
the bindings from applying the rest of the pattern to last-rest.

or: start from the end, and work backwards? (this is what i end up doing)
|#

; what do we need to forbid?
#; (p... a (p... b whatever))

; define the following as well:
#; (pcons <init-pat> <rest-pat>) ; where pa is just p-cons
#; (pl* <init-pats> ... <rest-pat>) ; like list*

; so the following are equivalent:
#; `(A B C ,d ... E ,f ... G H) 
#; `(pcons A (pcons B (pcons C (p... ,d (pcons E (p... ,f (pcons G (H))))))))
#; `(pl* A B C (p... d (pl* E (p... f (pl* G H)))))


#|
side note: for explaining pattern matching; the need to distiguish
datums/literals from pattern variables: take the analogy of expanding
a fn in source code. looking for a fn call, the fn name is a literal
and the parameters are captured as pattern variable (modulo evaluation
strategy of course; lead into discussion of macros)
|#

; this is the end. continued in fructerm!

