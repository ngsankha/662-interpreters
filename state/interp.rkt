#lang racket

(require "ast.rkt" "parser.rkt")

(provide interp-err)

;; interp-err :: Expr -> Val or Err
(define (interp-err e)
  (with-handlers ([Err? (λ (err) err)])
    (interp-prog e)))

;; interp :: Defn -> Env -> State -> Expr -> (Val, Env)
(define (interp D E S e)
  (match e
    [(Val v)         v]
    [(Var x)         (lookup D E S x)]
    [(UnOp u e)      (interp-unop D E S u e)]
    [(BinOp b e1 e2) (interp-binop D E S b e1 e2)]
    [(If e1 e2 e3)   (interp-if D E S e1 e2 e3)]
    [(Let x e1 e2)   (interp D (store E x (interp D E S e1)) S e2)]
    [(Lam xs e)      (interp-lam D E S xs e)]
    [(Defn x xs e)   (interp-lam D '() S xs e)]
    [(DefnV x e)     (interp D '() S e)]
    [(App e es)      (interp-app D E S e es)]
    [(Seq es)        (last (map (λ (e) (interp D E S e)) es))]
    [(New e)         (interp-new D E S e)]
    [(Deref e)       (interp-deref D E S e)]
    [(Set! e1 e2)    (interp-set! D E S e1 e2)]))

(define (interp-new D E S e)
  (let ([loc (gensym)]
        [v   (interp D E S e)])
    (begin
      (hash-set! S loc v)
      loc)))

(define (interp-deref D E S e)
  (let ([loc (interp D E S e)])
    (hash-ref S loc)))

(define (interp-set! D E S e1 e2)
  (let ([loc (interp D E S e1)]
        [v   (interp D E S e2)])
    (begin
      (hash-set! S loc v)
      v)))

;; interp-lam :: Defn -> Env -> Vars -> Expr -> Val
(define (interp-lam D E S xs body)
  (λ (aargs)
    (interp D (append (zip xs aargs) E) S body)))

;; interp-app :: Defn -> Env -> Expr -> Exprs -> Val
(define (interp-app D E S f es)
    (let ([fn   (interp D E S f)]
          [args (map (λ (arg) (interp D E S arg)) es)])
         (fn args)))

;; interp-prog :: Prog -> Val
(define (interp-prog prog)
  (match prog
    [(Prog D e) (interp D '() (make-hash) e)]))

;; interp-unop :: Defn -> Env -> UnOp -> Val
(define (interp-unop D E S u e)
  (match u
    ['add1  (match (interp D E S e)
              [(? integer? i) (add1 i)]
              [_              (raise (Err "add1 expects int"))])]
    ['sub1  (match (interp D E S e)
              [(? integer? i) (sub1 i)]
              [_              (raise (Err "sub1 expects int"))])]
    ['zero? (match (interp D E S e)
              [0 #t]
              [_ #f])]))

;; interp-binop :: Defn -> Env -> BinOp -> Expr -> Expr -> Val
(define (interp-binop D E S b e1 e2)
  (match b
    ['+ (match* ((interp D E S e1) (interp D E S e2))
          [((? integer? i1) (? integer? i2)) (+ i1 i2)]
          [(_ _)                             (raise (Err "+ requires int"))])]

    ['- (match* ((interp D E S e1) (interp D E S e2))
          [((? integer? i1) (? integer? i2)) (- i1 i2)]
          [(_ _)                             (raise (Err "- requires int"))])]

    ['* (match* ((interp D E S e1) (interp D E S e2))
          [((? integer? i1) (? integer? i2)) (* i1 i2)]
          [(_ _)                             (raise (Err "* requires int"))])]

    ['/ (match* ((interp D E S e1) (interp D E S e2))
          [((? integer? i1) (? integer? i2)) (if (eq? i2 0)
                                                 (raise (Err "division by 0 not allowed"))
                                                 (quotient i1 i2))]
          [(_ _)                             (raise (Err "/ requires int"))])]

    ['<= (match* ((interp D E S e1) (interp D E S e2))
          [((? integer? i1) (? integer? i2)) (<= i1 i2)]
          [(_ _)                             (raise (Err "<= requires int"))])]

    ['and (match (interp D E S e1)
            [#f #f]
            [?  (interp D E S e2)])]))

;; interp-if :: Defn -> Env -> Expr -> Expr -> Expr -> Val
(define (interp-if D E S e1 e2 e3)
  (match (interp D E S e1)
    [#f (interp D E S e3)]
    [_  (interp D E S e2)]))

(define zip (lambda (l1 l2) (map list l1 l2)))

;; store :: Env -> Symbol -> Val -> Env
(define (store E x v)
  (cons (list x v) E))

;; lookup :: Defn -> Env -> Symbol -> Val
(define (lookup D E S x)
  ; lookup the environment first, then the list of definitions
  (match E
    ['()                      (lookup-defn D E S D x)]
    [(cons (list y val) rest) (if (eq? x y) val
                                  (lookup D rest S x))]))

;; lookup-defn :: Defn -> Defn -> Symbol -> Val
(define (lookup-defn D E S defns x)
  (match defns
    ['()                          (raise (Err (string-append "Unbound identifier: "
                                                             (symbol->string x))))]
    [(cons (Defn f xs body) rest) (if (eq? f x)
                                      (interp D E S (Defn f xs body))
                                      (lookup-defn D E S rest x))]
    [(cons (DefnV y e) rest)      (if (eq? x y)
                                      (interp D E S (DefnV y e))
                                      (lookup-defn D E S rest x))]))
