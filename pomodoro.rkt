#lang racket/gui
(define (current-posix-seconds)
  (/ (inexact->exact (current-inexact-milliseconds)) 1000))

(define frame (new frame% [label "Timer"]))
(define msg (new message% [parent frame]
                 [label "Press Start button to start the timer."]))

; begin is the time when "Start" button is pressed (in milliseconds)
; diff is (- (current-inexact-milliseconds) begin)
(define begin 0)
(define diff 0)

; Timer callback
(define timer (new timer%
                   [notify-callback (lambda ()
                                               (set! diff (- (current-seconds) begin))
                                               (send msg set-label (number->string diff)))]
                   [interval #f]))

; Start button
(new button% [parent frame] [label "Start"]
     (callback (lambda (button event)
                 (set! begin (- (current-seconds) diff))
                 (send timer start 10))))

; Stop button
(new button% [parent frame]
     [label "Stop"]
     (callback (lambda (button event)
                 (send timer stop))))

; Reset button
(new button% [parent frame]
     [label "Clear"]
     (callback (lambda (button event)
                 (set! diff 0)
                 (set! begin (current-seconds))
                 (send msg set-label (number->string diff))
                 )))

; show window
(send frame show #t)