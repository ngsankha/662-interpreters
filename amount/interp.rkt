#lang racket

(require rackunit "ast.rkt" "parser.rkt")

(provide interp)

;; interp :: Expr -> Int
(define (interp e)
  (match e
    [(Int i) i]))

(define (check-interp e)
  (check-eqv? (interp (parse e))
              (eval e (make-base-namespace))))

(module+ test
  (check-eqv? (interp (parse 42)) 42)

  ; random testing
  (for ([i (in-range 10)])
    (check-interp (random 100000))))
