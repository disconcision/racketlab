#lang racket
(require 2htdp/image)
(require rackunit)


(provide render)

(define a "ssssssss ssssssssssssss")
(define char-height 24)
(define char-width (/ char-height 2))
(define space-width 12)
(define (text-width a)
  (* char-width (string-length a)))

(overlay (text a char-height "black")
         (rectangle (text-width a) char-height "solid" "white"))

(struct fruct (attributes children) #:transparent)

(define (sexpr->fruct stx)
  (match stx
    [(? symbol?) (fruct (hash 'sort 'atomic) stx)]
    [(? list?) (fruct (hash 'sort 'list) (map sexpr->fruct stx))]))

(check-equal? (sexpr->fruct '(atom1 (atom2 atom3) atom4))
              (fruct '#hash((sort . list))
                     (list
                      (fruct '#hash((sort . atomic)) 'atom1)
                      (fruct '#hash((sort . list))
                             (list
                              (fruct '#hash((sort . atomic)) 'atom2)
                              (fruct '#hash((sort . atomic)) 'atom3)))
                      (fruct '#hash((sort . atomic)) 'atom4))))

(define (fruct->sexpr f)
  (match f
    [(fruct (hash-table ('sort 'atomic)) whatever)
     whatever]
    [(fruct (hash-table (sort list)) whatever)
     (map fruct->sexpr whatever)]))

(check-equal? (fruct->sexpr (sexpr->fruct '(atom1 (atom2 atom3) atom4)))
              '(atom1 (atom2 atom3) atom4))

(define (fruct-width f)
  (match f
    [(fruct (hash-table ('sort 'atomic)) whatever)
     (* char-width (string-length (symbol->string whatever)))]
    [(fruct (hash-table (sort list)) whatever)
     (+ (apply + (map fruct-width whatever))
        (* space-width (+ -1 (length whatever))))]))

(fruct-width (sexpr->fruct '(atom1 (atom2 atom3) aom4)))



(define (add-widths f)
  (match f
    [(fruct (and h (hash-table ('sort 'atomic))) whatever)
     (fruct (hash-set h 'width (fruct-width f)) whatever)]
    [(fruct (and h (hash-table (sort list))) whatever)
     (fruct (hash-set h 'width (fruct-width f)) (map add-widths whatever))]))

(add-widths (sexpr->fruct '(atom1 (atom2 atom3) aom4)))



(define identifier-color (make-color 150 155 205))
(define datum-color (make-color 0 120 240))
(define identifier-outline-color (make-color 150 155 205))
;(define identifier-color (make-color 0 255 255))
(define app-color (make-color 250 40 220))
(define lambda-color (make-color 0 220 0))
(define let-color (make-color 0 255 255))

(define selection-color (make-color 0 100 255))
(define hole-color (make-color 200 200 200))
(define zero-color (make-color 0 0 0))

#; (define (box-project fruct)
     (match fruct
       [`(◇ ,a) (box-project a)]
       [`(p/ ,anns ,stx)
        (render stx)]
       #;[_ (println fruct)]))



#| render : stx → image |#
(define (render fruct)
  (match fruct
    [`(◇ ,a) (render a)]

    [`(p/ ,(hash-table ('▹ _) ('sort sort)) 0)
     (above (text (symbol->string '▹0) 24 selection-color)
            (text (string-upcase (symbol->string sort)) 8 (make-color 200 0 0)))]
    [`(p/ ,_ 0)
     (text "0" 18 zero-color)]
    
    [`(p/ ,(hash-table ('▹ _) ('sort sort)) ⊙)
     (above (text (symbol->string '▹⊙) 24 selection-color)
            (text (string-upcase (symbol->string sort)) 8 (make-color 200 0 0)))]
    [`(p/ ,_ ⊙)
     (text (symbol->string '⊙) 18 hole-color)]
    
    [`(p/ ,(hash-table ('▹ _)) (app ,f ,as ...))
     (println "yeh")
        (render-app selection-color #t `(app ,f ,@as))]
    [`(p/ ,_ (app ,f ,as ...))
        (render-app app-color #f `(app ,f ,@as))]
    
    [`(p/ ,anns ,stx)
        (render stx)]
    
    [(? number?)
     (render-datum fruct)]
    [(? symbol?)
     (render-identifier fruct)]
    [`(app ,f ,as ...)
     (render-app app-color #f fruct)]
    [`(λ ,p ,xs ...)
     (render-λ fruct)]
    [`(let ,c ,xs ...)
     (render-let fruct)]))

; rendering helpers
(define (render-list-horizontal stx)
  (match (length stx)
    [0 empty-image]
    [1 (first (map render stx))]
    [_ (apply beside/align* "center" (map render stx))]))
(define (render-list-vertical stx)
  (match (length stx)
    [0 empty-image]
    [1 (first (map render stx))]
    [_ (apply above/align "left" (map render stx))]))
(define (outline-image color img)
  (overlay/align
   "left" "center"
   img
   (rectangle (image-width img)
              (+ 4 (image-height img))
              "outline" color)))
(define (indent-image-horizontal width image)
  (overlay/offset 
   (rectangle (+ width (image-width image))
              (image-height image)
              "solid" (make-color 0 255 0 0))
   width 0
   image))
(define (beside/align* alignment . ls)
  (match (length ls)
    [0 empty-image]
    [1 (first ls)]
    [_ (apply beside/align alignment ls)]))
(define (list-above/align alignment . ls)
  (match (length ls)
    [0 empty-image]
    [1 (first ls)]
    [_ (apply above/align alignment ls)]))
(define (outline-vertical-header-indent-body
         color width
         first-head rest-head body)
  (outline-image
   color
   (above/align
    "left"
    (beside/align "center" first-head rest-head)
    (indent-image-horizontal width body))))

(define (render-datum stx)
  (text (~a stx) char-height datum-color))

(define (render-identifier stx)
  (text (~a stx) char-height identifier-color))

(define (render-app color selected? stx)
  (match stx
    [`(app ,f ,as ...)
     (outline-image
      color
      (beside/align*
       "center"
       (if selected?
           (above (beside (text (~a "▹") 24 color) (text (~a " app ") 12 color))
                  (text (string-upcase (symbol->string 'expr)) 8 (make-color 200 0 0)))
           (text (~a " app ") 12 color))
       (render f) ; what if we want to impose a color on function-term somehow?
       (render unit-spacer)
       (apply beside/align* "center" (map (λ (x) (render-list-horizontal `(,x ,unit-spacer))) as))))]))

(define (render-λ stx)
  (match stx
    [`(λ (,params ...) ,exprs ...)
     (outline-vertical-header-indent-body
      lambda-color 8
      (text (~a " fun ") 12 lambda-color)
      (outline-image lambda-color
                     (render-list-horizontal params))
      (render-list-vertical exprs))]))

(define unit-spacer
  '| |)

(define (render-let stx)
  (match stx
    [`(let ([,ids ,inits] ...) ,exprs ...)
     (outline-vertical-header-indent-body
      let-color 8
      (text (~a " let ") 12 let-color)
      (apply list-above/align
             "left"
             (for/list ([clause (map (λ (x y) `(,unit-spacer ,x  ,unit-spacer ,y ,unit-spacer)) ids inits)])
               (outline-image
                let-color
                (render-list-horizontal clause))))
      (render-list-vertical exprs))]))

(render 'dfg)
(render '(app f 1 (app g 5 6)))
(render '(λ (a b) 1))
(render '(let ([a 1][b 2]) a))
(render '(let ([a (app f 4 5)][b (app g (app h 1))])
           (let ([c 7])
             (app (λ (x y z) (app a b c)) 7 8 9))))

#|

identifier layout:
id-symbol

function call layout:
horizontal free-list
  <f>
  <a>
  ...

lambda layout:
vertical free-list
 horizontal free-list
   <λ-symbol>
   horizontal free-list
     <id>
     ...
 horizontal free-list
   <SPACER>
   <body1>
 ... 

let layout:
vertical free-list
 horizontal free-list
   <let-symbol>
   vertical free-list
     horizontal free-list
       <id>
       <init>
     ...
 horizontal free-list
   <SPACER>
   <body1>
 ...

|#
