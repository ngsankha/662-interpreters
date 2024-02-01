#lang racket

(require rackunit "ast.rkt" "parser.rkt")

(provide interp)

;; interp :: Expr -> Int
(define (interp e)
  (match e
    [(Int i) i]
    [(UnOp u e) (interp-unop u (interp e))]
    [(BinOp b e1 e2) (interp-binop b (interp e1) (interp e2))]))

(define (interp-unop u i)
  (match u
    ['add1 (add1 i)]
    ['sub1 (sub1 i)]))

(define (interp-binop b i1 i2)
  (match b
    ['+ (+ i1 i2)]
    ['- (- i1 i2)]
    ['* (* i1 i2)]
    ['/ (/ i1 i2)]))


(define (check-interp e)
  (check-eqv? (interp (parse e))
              (eval e (make-base-namespace))))

(define (random-expr)
   (contract-random-generate
    (flat-rec-contract b
                       (list/c 'add1 b)
                       (list/c 'sub1 b)
                       (list/c '+ b b)
                       (list/c '- b b)
                       (list/c '* b b)
                       (list/c '/ b b)
                       (integer-in #f #f))))

(module+ test
  (check-eqv? (interp (parse '(- (sub1 45) (add1 -8)))) 51)

  ; random testing
  (for ([i (in-range 10)])
    (check-interp (random-expr))))
