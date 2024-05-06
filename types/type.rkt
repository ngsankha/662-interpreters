#lang racket

(require "ast.rkt" "parser.rkt")

(provide tc)

;; tc :: TEnv -> Expr -> Type
(define (tc TE e)
  (match e
    [(Val v)         (tc-val v)]
    [(Var x)         (lookup TE x)]
    [(UnOp u e)      (tc-unop TE u e)]
    [(BinOp b e1 e2) (tc-binop TE b e1 e2)]
    [(If e1 e2 e3)   (tc-if TE e1 e2 e3)]
    [(Let x t e1 e2) (tc-let TE x t e1 e2)]
    [(Lam xts t e)   (tc-lam TE xts t e)]
    [(List es)       (tc-list TE es)]
    [(Map e1 e2)     (tc-map TE e1 e2)]
    [(App e es)      (tc-app TE e es)]))

(define (tc-val v)
  (match v
    [(? integer?) (T 'int)]
    [(? boolean?) (T 'bool)]
    [_            (error "Unexpected value")]))

(define (tc-unop TE u e)
  (match* (u (tc TE e))
    [('add1  (T 'int)) (T 'int)]
    [('sub1  (T 'int)) (T 'int)]
    [('zero? (T 'int)) (T 'bool)]
    [(_      _)        (error "Type error!")]))

(define (tc-binop TE b e1 e2)
  (match* (b (tc TE e1) (tc TE e2))
    [('+   (T 'int)  (T 'int))  (T 'int)]
    [('-   (T 'int)  (T 'int))  (T 'int)]
    [('*   (T 'int)  (T 'int))  (T 'int)]
    [('/   (T 'int)  (T 'int))  (T 'int)]
    [('<=  (T 'int)  (T 'int))  (T 'bool)]
    [('and (T 'bool) (T 'bool)) (T 'bool)]
    [(_    _         _)         (error "Type error!")]))

(define (tc-if TE e1 e2 e3)
  (match* ((tc TE e1) (tc TE e2) (tc TE e3))
    [((T 'bool) t2 t3) (union t2 t3)]
    [(_         _  _)  (error "Type error!")]))

(define (tc-let TE x t e1 e2)
  (if (equal? (tc TE e1) t)
      (tc (store TE x t) e2)
      (error "Type error!")))

(define (tc-lam TE xts t e)
  (if (equal? (tc (append xts TE) e) t)
      (FnT (map last xts) t)
      (error "Type error!")))

(define (tc-app TE e es)
  (match (tc TE e)
    [(FnT args ret) (if (equal? args (map (λ (e) (tc TE e)) es))
                        ret
                        (error "Type error!"))]
    [_              (error "Type error!")]))

(define (tc-list TE es)
  (let ((ts (map (λ (e) (tc TE e)) es)))
    (ParamT (T 'list) (foldl union (first ts) ts))))

(define (tc-map TE e1 e2)
  (match* ((tc TE e1) (tc TE e2))
    [((FnT (list xt) ret) (ParamT (T 'list) p)) (if (equal? xt p)
                                                    (ParamT (T 'list) ret)
                                                    (error "Type error!"))]
    [(_                   _)                    (error "Type error!")]))

(define zip (lambda (l1 l2) (map list l1 l2)))

(define (union t1 t2)
  (if (equal? t1 t2)
      t1
      (UnionT t1 t2)))

;; store :: TEnv -> Symbol -> Type -> Env
(define (store TE x t)
  (cons (list x t) TE))

;; lookup :: Defn -> TEnv -> Symbol -> Type
(define (lookup TE x)
  (match TE
    ['()                      (raise (Err (string-append "Unbound identifier: "
                                                         (symbol->string x))))]
    [(cons (list y val) rest) (if (equal? x y) val
                                  (lookup rest x))]))

(module+ test
  (require rackunit)

  (check-equal? (tc '() (parse '(let ((x : int 5))
                              (+ x 5)))) (T 'int))

  
  (check-equal? (tc '() (parse '((λ (x : int y : int) : int
                                   (+ x y)) 4 5))) (T 'int)))
