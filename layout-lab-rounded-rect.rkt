#lang racket

(require 2htdp/image
         lang/posn)


(define (rounded-rectangle w h init-radius my-color)
  ; added rounding to try and fix subpixel layout issues
  ; doesn't seem to have done much. todo: check if it does anything
  (define width (inexact->exact (round w)))
  (define height (inexact->exact (round h)))
  (define radius
    (inexact->exact
     (round
      (if (width . < . (* 2 init-radius))
          (/ width 2) ; note possible syntax issue
          init-radius))))
  
  (define pen
    (make-pen my-color (* 2 radius) "solid" "round" "round"))
  (underlay/align
   "middle" "middle"
   ; bounding box
   (rectangle width height "solid" (color 0 0 0 0))
   ; fill in inside
   (rectangle (max 0 (- width (* 2 radius)))
              (max 0 (- height (* 2 radius)))
              "solid" my-color)
   (polygon
    (list (make-posn radius radius)
          (make-posn (- width radius) radius)
          (make-posn (- width radius) (- height radius))
          (make-posn radius (- height radius)))
    "outline" pen)))


(define (rounded-rectangle-2 width height radius my-color)
  (define my-radius (inexact->exact (round radius)))
  (define corner
    (crop/align "left" "top" my-radius my-radius
                (circle my-radius "solid" my-color)))
  (define top-bar (rectangle (max 0 (- width (* 2 my-radius)))
                             my-radius  "solid" my-color))
  (define top-side (beside corner top-bar (rotate -90 corner)))
  (above top-side
         (rectangle (image-width top-side)
                    (max 0 (- height (* 2 my-radius)))
                    "solid" my-color)
         (rotate 180 top-side)))




#;(define (bracket-h width my-color corner-radius rect)
    (underlay
     (rounded-rectangle (+ (image-width rect) (* 2 width))
                        (image-height rect)
                        corner-radius my-color)
     rect))


#;(define (bracket-hl width my-color corner-radius rect)
    (underlay/align "right" "middle"
                    (rounded-rectangle (+ (image-width rect) width)
                                       (image-height rect)
                                       corner-radius my-color)
                    rect))


#;(define (bracket-hr width my-color corner-radius rect)
    (underlay/align "left" "middle"
                    (rounded-rectangle (+ (image-width rect) width)
                                       (image-height rect)
                                       corner-radius my-color)
                    rect))


#;(define (bracket-ht height my-color corner-radius rect)
    (underlay/align "middle" "bottom"
                    (rounded-rectangle (image-width rect)
                                       (+ (image-height rect) height)
                                       corner-radius my-color)
                    rect))


#;(define (bracket-hb height my-color corner-radius rect)
    (underlay/align "middle" "top"
                    (rounded-rectangle (image-width rect)
                                       (+ (image-height rect) height)
                                       corner-radius my-color)
                    rect))



(local
  [(define r 10)
   (define t 0.39)
   (define lineh 20)
   (define sl 200)
   (define sl2 100)
   (define sl3 150)
   (define sl4 180)
   (define sh 300)
   (define my-color "red")
   (define mode "outline")]
  (polygon (list (make-pulled-point 0 0
                                    0 r
                                    t -45)
                 (make-pulled-point t 45
                                    r 0
                                    0 0)
                 
                 (make-pulled-point 0 0
                                    (+ r sl) 0
                                    t -45)
                 (make-pulled-point t 45
                                    (+ r r sl) r
                                    0 0)
                 
                 (make-pulled-point 0 0
                                    (+ r r sl) (+ r lineh)
                                    t -45)
                 (make-pulled-point t 45
                                    (+ r sl) (+ (* 2 r) lineh)
                                    0 0)
                   
                 (make-pulled-point 0 0
                                    (+ r r sl2) (+ (* 2 r) lineh)
                                    t 45) ; curve ccw
                 (make-pulled-point t -45
                                    (+ r sl2) (+ (* 3 r) lineh)
                                    0 0)
                 
                   
                 (make-pulled-point 0 0
                                    (+ r sl2) (+ (* 3 r) (* 2 lineh))
                                    t 45) ; curve ccw
                 (make-pulled-point t -45
                                    (+ r r sl2) (+ (* 4 r) (* 2 lineh))
                                    0 0)

                 (make-pulled-point 0 0
                                    (+ r sl3) (+ (* 4 r) (* 2 lineh))
                                    t -45)
                 (make-pulled-point t 45
                                    (+ r r sl3) (+ (* 5 r) (* 2 lineh))
                                    0 0)

                   
                 (make-pulled-point 0 0
                                    (+ r r sl3) (+ (* 5 r) (* 3 lineh))
                                    t 45) ; curve ccw
                 (make-pulled-point t -45
                                    (+ r sl4) (+ (* 6 r) (* 3 lineh))
                                    0 0)
                   
                 (make-pulled-point 0 0
                                    (+ r sl4) (+ (* 6 r) (* 3 lineh))
                                    t -45)
                 (make-pulled-point t 45
                                    (+ r r sl4) (+ (* 7 r) (* 3 lineh))
                                    0 0)

                 (make-pulled-point 0 0
                                    (+ r r sl4) (+ (* 7 r) (* 4 lineh))
                                    t -45)
                 (make-pulled-point t 45
                                    (+ r sl4) (+ (* 8 r) (* 4 lineh))
                                    0 0)

                 (make-pulled-point 0 0
                                    r (+ (* 8 r) (* 4 lineh))
                                    t -45)
                 (make-pulled-point t 45
                                    0 (+ (* 7 r) (* 4 lineh))
                                    0 0)
                 )
           mode
           my-color))



(local
  [(define r 10)
   (define t 0.39)
   (define lineh 20)
   (define sl1 300)
   (define sl2 100)
   (define sl3 150)
   (define sl4 250)
   (define sh 300)
   (define my-color "red")
   (define mode "outline")
   (define (ff a b c d sl h cw?)
     (list (make-pulled-point 0 0
                              (+ (* a r) sl) (+ (* b r) h)
                              t (if (equal? 'cw cw?) -45 45))
           (make-pulled-point t (if (equal? 'cw cw?) 45 -45)
                              (+ (* c r) sl) (+ (* d r) h)
                              0 0)))]
  (polygon (append
            (list (make-pulled-point 0 0
                                     0 r
                                     t -45)
                  (make-pulled-point t 45
                                     r 0
                                     0 0)
                 
                  (make-pulled-point 0 0
                                     (+ (* 1 r) sl1) 0
                                     t -45)
                  (make-pulled-point t 45
                                     (+ (* 2 r) sl1) r
                                     0 0)
                  )
            (ff 2 1 1 2 sl1 (* 1 lineh) 'cw)
            (ff 2 2 1 3 sl2 (* 1 lineh) 'ccw)
              
            (ff 1 3 2 4 sl2 (* 2 lineh) 'ccw)
            (ff 1 4 2 5 sl3 (* 2 lineh) 'cw)

            (ff 2 5 3 6 sl3 (* 3 lineh) 'ccw)
            (ff 2 6 3 7 sl4 (* 3 lineh) 'cw)
              
            (ff 3 7 2 8 sl4 (* 4 lineh) 'cw)
            (list
             (make-pulled-point 0 0
                                r (+ (* 8 r) (* 4 lineh))
                                t -45)
             (make-pulled-point t 45
                                0 (+ (* 7 r) (* 4 lineh))
                                0 0)
             )
            )
           mode
           my-color))


(local
  [(define r 10)
   (define t 0.39)
   (define lineh 20)
   (define sl0 0)
   (define sl1 300)
   (define sl2 100)
   (define sl3 150)
   (define sl4 250)
   (define sln 0)
   (define sh 300)
   (define my-color "red")
   (define mode "outline")
   (define (ff a c b d sl h cw?)
     (list (make-pulled-point 0 0
                              (+ (* a r) sl) (+ (* b r) h)
                              t (if (equal? 'cw cw?) -45 45))
           (make-pulled-point t (if (equal? 'cw cw?) 45 -45)
                              (+ (* c r) sl) (+ (* d r) h)
                              0 0)))]
  (polygon (append
             
              
              
            (ff -1  0 0 1 sl1 (* 0 lineh) 'cw)
            (ff  0 -1 1 2 sl1 (* 1 lineh) 'cw)
              
            (ff  0 -1 2 3 sl2 (* 1 lineh) 'ccw)
            (ff -1  0 3 4 sl2 (* 2 lineh) 'ccw)
              
            (ff -1  0 4 5 sl3 (* 2 lineh) 'cw)
            (ff  0  1 5 6 sl3 (* 3 lineh) 'ccw)
              
            (ff -1  0 6 7 sl4 (* 3 lineh) 'cw)
            (ff  0 -1 7 8 sl4 (* 4 lineh) 'cw)

            (ff  1  0 8 7 sln (* 4 lineh) 'cw)
              
            )
           mode
           my-color))


(define (make-backing source-rows r my-color mode)
  ; enforce precondition:
  ; radius is no greater than 1/2 min row height/width
  ; this is bugged right now; radius is getting added to height
  #;(define r (apply min init-r
                   (map (λ (x) (inexact->exact (round (* 1/2 x))))
                        (apply append source-rows))))
  (define t 0.39)
  (define (corner a c sl b d h rot)
    (list (make-pulled-point 0 0
                             (+ (* a r) sl) (+ (* b r) h)
                             t rot)
          (make-pulled-point t (- rot)
                             (+ (* c r) sl) (+ (* d r) h)
                             0 0)))
  (define (row n sl ltp ltn init-h final-h)
    ; ltp - longer than previous
    ; ltn - longer than next
    (append
     (corner (- ltp) 0 sl
             (+ 0 (* 2 n)) (+ 1 (* 2 n)) init-h
             (* ltp -45))
     (corner 0 (- ltn) sl
             (+ 1 (* 2 n)) (+ 2 (* 2 n)) final-h
             (* ltn -45))))
  (define (left-side num-rows total-height)
    (append
     (corner 1 0 0 (* 2 num-rows) (+ -1 (* 2 num-rows)) total-height -45)
     (corner 0 1 0 1 0 0 -45)))
  (define (3> a b)
    (cond
      [(> a b) 1]
      [(= a b) 0]
      [else -1]))
  (define-values (rows-with-ltp _)
    (for/fold ([acc '()]
               [prev-w 0])
              ([r source-rows])
      (match-define (list w h) r)
      (values `(,@acc ,(list w h (3> w prev-w))) w)))
  (define-values (rows-with-ltp-ltn __)
    (for/fold ([acc '()]
               [prev-w 0])
              ([r (reverse rows-with-ltp)])
      (match-define (list w h ltp) r)
      (values `(,@acc ,(list w h ltp (3> w prev-w))) w)))
  (define rows-with-both-augs
    (reverse rows-with-ltp-ltn))
  (define-values (rows num-rows total-h)
    (for/fold ([acc '()]
               [n 0]
               [h 0])
              ([r rows-with-both-augs])
      (match-define (list c-w c-h ltp ltn) r)
      (values `(,@acc ,(row n c-w ltp ltn h (+ h c-h)))
              (add1 n)
              (+ h c-h))))
  (polygon (append
            (apply append rows)
            (left-side num-rows total-h))
           mode
           my-color))

(make-backing (list '(300 0)
                    '(100 20)
                    '(400 20)
                    '(400 20)
                    '(300 20)
                    '(150 20)
                    '(250 20))
              10 "red" "outline")