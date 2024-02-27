#lang racket

(provide main)

(require "parser.rkt" "interp.rkt")

(define (main fn)
  (let ([p (open-input-file fn)])
    (begin
      (read-line p) ;; ignore #lang racket line
      (println (interp-err (parse (read p))))
      (close-input-port p))))
