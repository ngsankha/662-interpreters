#lang racket

(provide main)

(require "parser.rkt" "interp.rkt")

;; read all s-expression until eof
(define (read-all p)
  (let ((r (read p)))
    (if (eof-object? r)
        '()
        (cons r (read-all p)))))

(define (main fn)
  (let ([p (open-input-file fn)])
    (begin
      (read-line p) ;; ignore #lang racket line
      (let ((r (interp-err (parse-prog (read-all p)))))
        (unless (void? r)
          (println r)))
      (close-input-port p))))
