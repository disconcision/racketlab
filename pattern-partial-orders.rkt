#lang racket




; pattern matching partial order?

; top: _ and pattern-identifiers

; pcons, p...

; not and or

#; (match* (big sub)
     [('_ _) #t]
     [((? pattern-id?) _) #t]
     [(`(pcons ,a ,b) `(pcons ,c ,d)) #t])