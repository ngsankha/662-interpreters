#lang racket

(require "ast.rkt" "parser.rkt")

;; Prog -> List Int
(define (find-ints e)
  (match e
    [(Val v)         (if (integer? v)
                         (list v)
                         '())]
    [(Var x)         '()]
    [(UnOp u e)      (find-ints e)]
    [(BinOp b e1 e2) (append (find-ints e1) (find-ints e2))]
    [(If e1 e2 e3)   (append (find-ints e1) (find-ints e2) (find-ints e3))]
    [(Let x e1 e2)   (append (find-ints e1) (find-ints e2))]
    [(Lam xs e)      (find-ints e)]
    [(Defn x xs e)   (find-ints e)]
    [(DefnV x e)     (find-ints e)]
    [(App e es)      (flatten (append (find-ints e) (map find-ints es)))]
    [(Prog ds e)     (flatten (append (find-ints e) (map find-ints ds)))]))

;; Prog -> Prog
(define (optimize e)
  (match e
    [(Val v)         (Val v)]
    [(Var x)         (Var x)]
    [(UnOp u e)      (UnOp u (optimize e))]
    [(BinOp b e1 e2) (optimize-binop b (optimize e1) (optimize e2))]
    [(If e1 e2 e3)   (If (optimize e1) (optimize e2) (optimize e3))]
    [(Let x e1 e2)   (App (Lam x (optimize e2)) (list (optimize e1)))]
    [(Lam xs e)      (Lam xs (optimize e))]
    [(Defn x xs e)   (Defn x xs (optimize e))]
    [(DefnV x e)     (DefnV x (optimize e))]
    [(App e es)      (App (optimize e) (map optimize es))]
    [(Prog ds e)     (Prog (map optimize ds) (optimize e))]))

(define (optimize-binop b e1 e2)
  (match b
    ['- (cond
          ;; e - e == 0
          [(equal? e1 e2)      (Val 0)]
          ;; e - 0 == e
          [(equal? e2 (Val 0)) e1]
          [else                (BinOp b e1 e2)])]
    [_ (BinOp b e1 e2)]))