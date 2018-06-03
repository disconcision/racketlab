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
#; (p-append a b) ; for lists a, b
#; (p-append a (p-append b whatever)) ; for lists a, b


#|
first, lets rewrite ...-containing variable-length list patterns:
pc = pcons: operates like cons  (first elem is elem, second elem is list)
pa = pappend: 
|#
#; `(A B C ,d ... E ,f ... G H) 
#; `(pl 'A (pl 'B (pl 'C (pa d (pl 'E (pa f (pl G '(H))))))))


#|
algo attempt 2:

|#

; sketch 1:
#; (match* (pat tem)
     [()])


