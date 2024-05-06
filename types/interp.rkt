#lang racket

(require "ast.rkt" "parser.rkt" "type.rkt")

(provide interp-err)

;; interp-err :: Expr -> Val
(define (interp-err e)
  (with-handlers ([Err? (λ (err) err)])
    (begin
      (tc '() e)
      (interp '() e))))

;; interp :: Env -> Expr -> Val
(define (interp E e)
  (match e
    [(Val v)         v]
    [(Var x)         (lookup E x)]
    [(UnOp u e)      (interp-unop E u e)]
    [(BinOp b e1 e2) (interp-binop E b e1 e2)]
    [(If e1 e2 e3)   (interp-if E e1 e2 e3)]
    [(Let x t e1 e2) (interp (store E x (interp E e1)) e2)]
    [(Lam xs t e)    (interp-lam E xs e)]
    [(List es)       (interp-list E es)]
    [(Map e1 e2)     (interp-map E e1 e2)]
    [(App e es)      (interp-app E e es)]))

(define (interp-map E e1 e2)
  (map (λ (v)
         ((interp E e1) (list v)))
       (interp E e2)))

(define (interp-list E es)
  (map (λ (e) (interp E e)) es))

;; interp-lam :: Env -> Vars -> Expr -> Val
(define (interp-lam E xs body)
  (λ (aargs)
    (interp (append (zip (map first xs) aargs) E) body)))

;; interp-app :: Env -> Expr -> Exprs -> Val
(define (interp-app E f es)
    (let ([fn   (interp E f)]
          [args (map (λ (arg) (interp E arg)) es)])
         (fn args)))

;; interp-unop :: Env -> UnOp -> Val
(define (interp-unop E u e)
  (match u
    ['add1  (match (interp E e)
              [(? integer? i) (add1 i)]
              [_              (raise (Err "add1 expects int"))])]
    ['sub1  (match (interp E e)
              [(? integer? i) (sub1 i)]
              [_              (raise (Err "sub1 expects int"))])]
    ['zero? (match (interp E e)
              [0 #t]
              [_ #f])]))

;; interp-binop :: Env -> BinOp -> Expr -> Expr -> Val
(define (interp-binop E b e1 e2)
  (match b
    ['+ (+ (interp E e1) (interp E e2))]

    ['- (match* ((interp E e1) (interp E e2))
          [((? integer? i1) (? integer? i2)) (- i1 i2)]
          [(_ _)                             (raise (Err "- requires int"))])]

    ['* (match* ((interp E e1) (interp E e2))
          [((? integer? i1) (? integer? i2)) (* i1 i2)]
          [(_ _)                             (raise (Err "* requires int"))])]

    ['/ (match* ((interp E e1) (interp E e2))
          [((? integer? i1) (? integer? i2)) (if (eq? i2 0)
                                                 (raise (Err "division by 0 not allowed"))
                                                 (quotient i1 i2))]
          [(_ _)                             (raise (Err "/ requires int"))])]

    ['<= (match* ((interp E e1) (interp E e2))
          [((? integer? i1) (? integer? i2)) (<= i1 i2)]
          [(_ _)                             (raise (Err "<= requires int"))])]

    ['and (match (interp E e1)
            [#f #f]
            [?  (interp E e2)])]))

;; interp-if :: Env -> Expr -> Expr -> Expr -> Val
(define (interp-if E e1 e2 e3)
  (match (interp E e1)
    [#f (interp E e3)]
    [_  (interp E e2)]))

(define zip (lambda (l1 l2) (map list l1 l2)))

;; store :: Env -> Symbol -> Val -> Env
(define (store E x v)
  (cons (list x v) E))

;; lookup :: Env -> Symbol -> Val
(define (lookup E x)
  (match E
    ['()                          (raise (Err (string-append "Unbound identifier: "
                                                             (symbol->string x))))]
    [(cons (list y val) rest) (if (eq? x y) val
                                  (lookup rest x))]))

(module+ test
  (require rackunit)

  (check-equal? (interp-err (parse '((λ (x : (-> int int) y : int) : int
                                     (x y)) (λ (y : int) : int (+ y 5)) 6))) 11)
  (check-equal? (interp-err (parse '(let ((l : (list int) (list 1 2 3 0)))
                                      (map (λ (v : int) : int v) l)))) '(1 2 3 0)))
