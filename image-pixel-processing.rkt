#lang racket

(require 2htdp/image
         lang/posn
         images/flomap
         racket/flonum
         (only-in racket/draw
                  bitmap%
                  bitmap-dc%))


(define-match-expander *flvector
  (lambda (stx)
    (syntax-case stx ()
      [(*flvector a b c d)
       #'(? flvector?
            (app (λ (x) (vector (flvector-ref x 0)
                                (flvector-ref x 1)
                                (flvector-ref x 2)
                                (flvector-ref x 3)))
                 (vector a b c d)))])))


(define (image->bitmap image)
  (let* ([width (image-width image)]
         [height (image-height image)]
         [bm (make-object bitmap% width height #f #t)]
         [dc (make-object bitmap-dc% bm)])
    #;(send dc clear)
    (send image draw dc 0 0 0 0 width height 0 0 #f)
    bm))


(define (apply-image-fn f image)
  (define image-flomap
    (bitmap->flomap (image->bitmap image)))
  (define new-flomap
    (build-flomap* 4 (image-width image) (image-height image)
                   (λ (x y) (f (flomap-ref* image-flomap x y)))))
  (flomap->bitmap new-flomap))


(define (per-flvector-linear-dodge-tint tint-color opacity)
  (match-define (vector r-tint g-tint b-tint _) tint-color)
  (define (per-channel-transformer x x-tint)
    (+ (* opacity x)
       (* (- 1 opacity) (min 1.0 (+ x x-tint)))))
  (match-lambda
    [(*flvector r g b a)
     (flvector (per-channel-transformer r r-tint)
               (per-channel-transformer g g-tint)
               (per-channel-transformer b b-tint)
               a)]))

(define (linear-dodge-tint-red-per-pixel fv)
  (flvector
   (flvector-ref fv 0)
   (+ (* 0.3 (flvector-ref fv 1))
      (* 0.7 (min 1.0 (+ (flvector-ref fv 1) 0.4))))
   (flvector-ref fv 2)
   (flvector-ref fv 3)))


(define (per-color-linear-dodge-tint tint-color opacity)
  (match-define (color r-tint g-tint b-tint _) tint-color)
  (define (per-channel-transformer x x-tint)
    (inexact->exact
     (round (+ (* opacity x)
               (* (- 1 opacity) (min 255 (+ x x-tint)))))))
  (match-lambda
    [(color r g b a)
     (color (per-channel-transformer r r-tint)
            (per-channel-transformer g g-tint)
            (per-channel-transformer b b-tint)
            a)]))


(define (linear-dodge-tint-red my-image)
  (apply-image-fn linear-dodge-tint-red-per-pixel my-image))


(define (linear-dodge-tint my-image tint-color opacity)
  (color-list->bitmap
   (map (per-color-linear-dodge-tint tint-color opacity)
        (image->color-list my-image))
   (image-width my-image) (image-height my-image)))



