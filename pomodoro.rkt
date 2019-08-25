#lang racket

(require racket/gui)
(require racket/serialize)
(require racket/stream)

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


;;;;;;;;;;;;;;
;; Pomodoro ;;
;;;;;;;;;;;;;;

;; The Pomodoro Technique is a time management method developed by
;; Francesco Cirillo in the late 1980s. The technique uses a
;; timer to break down work into intervals, traditionally 25
;; minutes in length, separated by short breaks. Each interval is
;; known as a pomodoro, from the Italian word for 'tomato',
;; after the tomato-shaped kitchen timer that Cirillo used as a
;; university student

;; The technique has been widely popularized by dozens of apps
;; and websites providing timers and instructions. Closely
;; related to concepts such as timeboxing and iterative
;; and incremental development used in software design,
;; the method has been adopted in pair programming contexts.


;; There are six steps in the original technique:

;; 1. Decide on the task to be done.
;; 2. Set the pomodoro timer (traditionally to 25 minutes).
;; 3. Work on the task.
;; 4. End work when the timer rings
;;    and put a checkmark on a piece of paper.
;; 5. If you have fewer than four checkmarks,
;;    take a short break (3–5 minutes),
;;    then go to step 2.
;; 6. After four pomodoros, take a longer break
;;    (15–30 minutes),
;;    reset your checkmark count to zero, then go to step 1.

;; The stages of planning, tracking, recording, processing
;; and visualizing are fundamental to the technique.
;; In the planning phase, tasks are prioritized by recording
;; them in a "To Do Today" list.
;; This enables users to estimate the effort tasks require.
;; As pomodoros are completed, they are recorded,
;; adding to a sense of accomplishment and providing raw data
;; for subsequent self-observation and improvement.


;; For the purposes of the technique, a pomodoro is the interval
;; of time spent working.
;; After task completion, any time remaining in the Pomodoro
;; is devoted to overlearning.
;; Regular breaks are taken, aiding assimilation. A short
;; (3–5 minutes) rest separates consecutive pomodoros.
;; Four pomodoros form a set.
;; A longer (15–30 minute) rest is taken between sets.

;; A goal of the technique is to reduce the impact of internal
;; and external interruptions on focus and flow.
;; A pomodoro is indivisible when interrupted during a pomodoro,
;; either the other activity must be recorded and
;; postponed
;; (using the inform – negotiate – schedule – call back strategy)
;; https://lifehacker.com/productivity-101-a-primer-to-the-pomodoro-technique-1598992730
;; or the pomodoro must be abandoned.


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
