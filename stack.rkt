#lang racket
(define (make-stack xs)
     (set! xs (list)))

(define (stack-push xs item)
   (set! xs (cons item xs)))

(define (stack-pop xs)
  (if
    (empty? xs) (error "Empty")
    (set! xs cdr)))
(define stack? list?)
(define stack-empty? empty?)

(define stack->top car)

(define stack-length length)

