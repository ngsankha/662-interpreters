#lang racket

(require rackunit "ast.rkt" "parser.rkt")

(provide interp-err)

;; interp-err :: Expr -> Val or Err
(define (interp-err e)
  (with-handlers ([Err? (λ (err) err)])
    (interp e)))

;; interp :: Expr -> Val
(define (interp e)
  (match e
    [(Val v) v]
    [(UnOp u e) (interp-unop u e)]
    [(BinOp b e1 e2) (interp-binop b e1 e2)]
    [(If e1 e2 e3) (interp-if e1 e2 e3)]))

;; interp-unop :: UnOp -> Val
(define (interp-unop u e)
  (match u
    ['add1 (match (interp e)
             [(? integer? i) (add1 i)]
             [_ (raise (Err "add1 expects int"))])]
    ['sub1 (match (interp e)
             [(? integer? i) (sub1 i)]
             [_ (raise (Err "sub1 expects int"))])]
    ['zero? (match (interp e)
              [0 #t]
              [_ #f])]))

;; interp-binop :: BinOp -> Val
(define (interp-binop b e1 e2)
  (match b
    ['+ (match* ((interp e1) (interp e2))
          [((? integer? i1) (? integer? i2)) (+ i1 i2)]
          [(_ _) (raise (Err "+ requires int"))])]
    ['- (match* ((interp e1) (interp e2))
          [((? integer? i1) (? integer? i2)) (- i1 i2)]
          [(_ _) (raise (Err "- requires int"))])]
    ['* (match* ((interp e1) (interp e2))
          [((? integer? i1) (? integer? i2)) (* i1 i2)]
          [(_ _) (raise (Err "* requires int"))])]
    ['/ (match* ((interp e1) (interp e2))
          [((? integer? i1) (? integer? i2)) (if (eq? i2 0)
                                                 (raise (Err "division by 0 not allowed"))
                                                 (quotient i1 i2))]
          [(_ _) (raise (Err "/ requires int"))])]
    ['<= (match* ((interp e1) (interp e2))
          [((? integer? i1) (? integer? i2)) (<= i1 i2)]
          [(_ _) (raise (Err "<= requires int"))])]
    ['and (match (interp e1)
            [#f #f]
            [? (interp e2)])]))

;; interp-if :: If -> Val
(define (interp-if e1 e2 e3)
  (match (interp e1)
    [#f (interp e3)]
    [_  (interp e2)]))

(module+ test
  (check-equal? (interp-err (parse '(+ 42 (sub1 34)))) 75)
  (check-equal? (interp-err (parse '(zero? (- 5 (sub1 6))))) #t)
  (check-equal? (interp-err (parse '(if (zero? 0) (add1 5) (sub1 5)))) 6)
  (check-equal? (interp-err (parse '(add1 (+ 3 #f))))
                (Err "+ requires int"))
  (check-equal? (interp-err (parse '(add1 (and #t #t))))
                (Err "add1 expects int"))
  (check-equal? (interp-err (parse '(/ 5 (sub1 1))))
                (Err "division by 0 not allowed")))
