#lang racket

(let ((x (add1 6)))
  (let ((x (+ 6 x)))
    (/ x 2)))
