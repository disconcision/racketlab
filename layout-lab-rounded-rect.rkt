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
