#lang racket
(require racket/gui)
(define (current-posix-seconds)
  (/ (inexact->exact (current-inexact-milliseconds)) 1000))
(define title "Timer")
(define frame (new frame% [label title]))
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
;text frame
(define text-field-new-task
  (new text-field%
       (label "New task")
       (parent frame)
       (init-value "")))

; Start button
(new button% [parent frame] [label "Start"]
     (callback (lambda (button event)
                 (set! begin (- (current-seconds) diff))
                 (send timer start 10)
                 (let ((new-task-value (string-normalize-spaces
                                          (send text-field-new-task get-value))))
                     (if (not (equal? new-task-value ""))
                         (begin
                           (send list-box-task append new-task-value)
                           (send (send text-field-new-task get-editor) erase))
                         (begin
                           (send text-field-new-task set-value "")
                           (message-box title  "Task name is mandatory" frame)))))))

; Reset button
(new button% [parent frame]
     [label "Clear"]
     (callback (lambda (button event)
                 (set! diff 0)
                 (set! begin (current-seconds))
                 (send msg set-label (number->string diff))
                 )))

(define group-box-panel-task-ToDo
  (new group-box-panel%
       (parent frame)
       (label "ToDo List")))

(define list-box-task
  (new list-box%
       (label "")
       (parent group-box-panel-task-ToDo)
       (choices (list))
       (style (list 'single))
       (columns (list ""))))

; Stop button
(new button% [parent group-box-panel-task-ToDo]
     [label "Stop"]
     (callback (lambda (button event)
                 (send timer stop)
                  (let ((index (send list-box-task get-selection))
                         (value (send list-box-task get-string-selection)))
                     (send list-box-task-completed append value)
                     (send list-box-task delete index)))))

(define group-box-panel-task-completed
  (new group-box-panel%
       (parent frame)
       (label "Task Completed")))
(define list-box-task-completed
  (new list-box%
       (label "")
       (parent group-box-panel-task-completed)
       (choices (list))
       (style (list 'single))
       (columns (list ""))))

; show window
(send frame show #t)