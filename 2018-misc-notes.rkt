#lang racket
'(🎾 🤣 😃 😄 😅 😆 😉 😊 😋 😎 😍 😘 😗
    😦 😧 😨 😩 🤯 😬 )


; Functions head and last could have been defined through folding as
; head = foldr (\x r -> x) (error "head: Empty list")
; last = foldl (\a x -> x) (error "last: Empty list")


(define ffirst
  (curry foldr (λ (a b) a) "error"))

(define llast
  (curry foldl (λ (x acc) x) "error"))

(ffirst '(1 2))
(llast '(1 2))