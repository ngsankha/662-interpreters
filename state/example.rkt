#lang racket

(let ((x (new 5)))
  (begin
    (set! x (add1 (deref x)))
    (+ 3 (deref x))))
