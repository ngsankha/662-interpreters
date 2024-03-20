#lang racket

(define (odd? x)
  (if (zero? x) #f
      (even? (sub1 x))))

(define (even? x)
  (if (zero? x) #t
      (odd? (sub1 x))))

(odd? 42)
