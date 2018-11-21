#lang racket
(require 2htdp/universe)



; GENERAL TRANSITION LOGIC
; on-draw:
; if state.transcounter == 0  draw-state
; if state.transcounter != 0  draw-tween(transcounter)
; on-tick:
; decrement state.transcounter
; on-key:
; (optionally) set state.transcounter to transition-length

; ?? where to store per-transformation transition fns?
; (optionally) set state.draw-tween to transition-tween

; NEW STUFF NEEDED:
; draw-tween(t) where t \in {transition-length,0} satisying:
; draw-tween(transition-length) == draw-state(previous-state)
; draw-tween(0) == draw-state(new-state)
; state.transcounter, initially 0
; and each transform with a transition needs a transition-length
; other transforms should be unaffected



; EXPERIMENT 0:
; slowly flashing ball


; EXPERIMENT 1:
; two actual states (room ball empty)
; (room empty ball)
; when press down starts transition animation


; EXPERIMENT 2:
; two actual state
; with different position/sizes of a rectangle
; tween frames smoothly take one rectangle to the other