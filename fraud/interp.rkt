#lang racket

(require rackunit "ast.rkt" "parser.rkt")

(provide interp-err)

;; interp-err :: Expr -> Val or Err
(define (interp-err e)
  (with-handlers ([Err? (λ (err) err)])
    ; '() is the empty environment
    (interp '() e)))

;; interp :: Env -> Expr -> Val
(define (interp env e)
  (match e
    [(Val v) v]
    [(Var x) (lookup env x)]
    [(UnOp u e) (interp-unop env u e)]
    [(BinOp b e1 e2) (interp-binop env b e1 e2)]
    [(If e1 e2 e3) (interp-if env e1 e2 e3)]
    [(Let x e1 e2) (interp
                    (store env x             ; env will be updated
                           (interp env e1))  ; after e1 is evaled in old env
                    e2)]))                   ; e2 evaluated in updated env

;; interp-unop :: Env -> UnOp -> Val
(define (interp-unop env u e)
  (match u
    ['add1 (match (interp env e)
             [(? integer? i) (add1 i)]
             [_ (raise (Err "add1 expects int"))])]
    ['sub1 (match (interp env e)
             [(? integer? i) (sub1 i)]
             [_ (raise (Err "sub1 expects int"))])]
    ['zero? (match (interp env e)
              [0 #t]
              [_ #f])]))

;; interp-binop :: Env -> BinOp -> Val
(define (interp-binop env b e1 e2)
  (match b
    ['+ (match* ((interp env e1) (interp env e2))
          [((? integer? i1) (? integer? i2)) (+ i1 i2)]
          [(_ _) (raise (Err "+ requires int"))])]
    ['- (match* ((interp env e1) (interp env e2))
          [((? integer? i1) (? integer? i2)) (- i1 i2)]
          [(_ _) (raise (Err "- requires int"))])]
    ['* (match* ((interp env e1) (interp env e2))
          [((? integer? i1) (? integer? i2)) (* i1 i2)]
          [(_ _) (raise (Err "* requires int"))])]
    ['/ (match* ((interp env e1) (interp env e2))
          [((? integer? i1) (? integer? i2)) (if (eq? i2 0)
                                                 (raise (Err "division by 0 not allowed"))
                                                 (quotient i1 i2))]
          [(_ _) (raise (Err "/ requires int"))])]
    ['<= (match* ((interp env e1) (interp env e2))
          [((? integer? i1) (? integer? i2)) (<= i1 i2)]
          [(_ _) (raise (Err "<= requires int"))])]
    ['and (match (interp env e1)
            [#f #f]
            [? (interp env e2)])]))

;; interp-if :: If -> Val
(define (interp-if env e1 e2 e3)
  (match (interp env e1)
    [#f (interp env e3)]
    [_  (interp env e2)]))

;; store :: Env -> Symbol -> Val -> Env 
(define (store env x val)
  (cons (list x val) env))

;; lookup :: Env -> Symbol -> Val
(define (lookup env x)
  (match env
    ['() (raise (Err "unbound identifier"))]
    [(cons (list y val) rest) (if (eq? x y) val
                                  (lookup rest x))]))

(module+ test
  (check-equal? (interp-err (parse '(+ 42 (sub1 34)))) 75)
  (check-equal? (interp-err (parse '(zero? (- 5 (sub1 6))))) #t)
  (check-equal? (interp-err (parse '(if (zero? 0) (add1 5) (sub1 5)))) 6)
  (check-equal? (interp-err (parse '(add1 (+ 3 #f))))
                (Err "+ requires int"))
  (check-equal? (interp-err (parse '(add1 (and #t #t))))
                (Err "add1 expects int"))
  (check-equal? (interp-err (parse '(/ 5 (sub1 1))))
                (Err "division by 0 not allowed"))
  (check-equal? (interp-err (parse '(let ((x 1)) (+ x 3)))) 4)
  (check-equal? (interp-err (parse '(let ((x 1))
                                      (let ((y 2))
                                        (+ x y))))) 3)
  (check-equal? (interp-err (parse '(let ((x (add1 6)))
                                      (let ((x (+ 6 x)))
                                        (/ x 2))))) 6)
  (check-equal? (interp-err (parse '(let ((x (add1 6)))
                                      (let ((x (+ 6 x)))
                                        (/ x y)))))
                (Err "unbound identifier")))
