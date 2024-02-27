#lang racket

(require rackunit "ast.rkt" "parser.rkt")

(provide interp-err)

(define zip (λ (l1 l2) (map list l1 l2)))

;; interp-err :: Expr -> Val or Err
(define (interp-err e)
  (with-handlers ([Err? (λ (err) err)])
    (interp-prog e)))

;; interp :: Defns -> Env -> Expr -> Val
(define (interp defn env e)
  (match e
    [(Val v) v]
    [(Var x) (lookup env x)]
    [(UnOp u e) (interp-unop defn env u e)]
    [(BinOp b e1 e2) (interp-binop defn env b e1 e2)]
    [(If e1 e2 e3) (interp-if defn env e1 e2 e3)]
    [(Let x e1 e2) (interp defn
                    (store env x (interp defn env e1))
                    e2)]
    [(App f actual) (match (lookup-defn f defn)
                    [(cons formal body) (interp defn
                                                (mk-local-env defn env (zip formal actual))
                                                body)])]))

;; mk-local-env :: Listof Defn -> Env -> Listof (Symbol, Expr)
(define (mk-local-env defn env params)
  (match params
    ['() env]
    [(cons (list x y) rest) (mk-local-env defn (store env x (interp defn env y)) rest)]))

;; lookup-defn :: Symbol -> Listof Defn -> (Symbols, Expr)
(define (lookup-defn f defns)
  (match defns
    ['() (raise (Err (string-append "Definition not found: " (symbol->string f))))]
    [(cons d rest) (match d
                     [(Defn name args body) (if (eq? name f)
                                                (cons args body)
                                                (lookup-defn f rest))])]))

;; interp-prog :: Prog -> Val
(define (interp-prog prog)
  (match prog
    ; '() is the empty environment
    [(Prog defns e) (interp defns '() e)]))

;; interp-unop :: Defns -> Env -> UnOp -> Val
(define (interp-unop defn env u e)
  (match u
    ['add1 (match (interp defn env e)
             [(? integer? i) (add1 i)]
             [_ (raise (Err "add1 expects int"))])]
    ['sub1 (match (interp defn env e)
             [(? integer? i) (sub1 i)]
             [_ (raise (Err "sub1 expects int"))])]
    ['zero? (match (interp defn env e)
              [0 #t]
              [_ #f])]))

;; interp-binop :: Defns -> Env -> BinOp -> Val
(define (interp-binop defn env b e1 e2)
  (match b
    ['+ (match* ((interp defn env e1) (interp defn env e2))
          [((? integer? i1) (? integer? i2)) (+ i1 i2)]
          [(_ _) (raise (Err "+ requires int"))])]
    ['- (match* ((interp defn env e1) (interp defn env e2))
          [((? integer? i1) (? integer? i2)) (- i1 i2)]
          [(_ _) (raise (Err "- requires int"))])]
    ['* (match* ((interp defn env e1) (interp defn env e2))
          [((? integer? i1) (? integer? i2)) (* i1 i2)]
          [(_ _) (raise (Err "* requires int"))])]
    ['/ (match* ((interp defn env e1) (interp defn env e2))
          [((? integer? i1) (? integer? i2)) (if (eq? i2 0)
                                                 (raise (Err "division by 0 not allowed"))
                                                 (quotient i1 i2))]
          [(_ _) (raise (Err "/ requires int"))])]
    ['<= (match* ((interp defn env e1) (interp defn env e2))
          [((? integer? i1) (? integer? i2)) (<= i1 i2)]
          [(_ _) (raise (Err "<= requires int"))])]
    ['and (match (interp defn env e1)
            [#f #f]
            [? (interp defn env e2)])]))

;; interp-if :: Defns -> Env -> If -> Val
(define (interp-if defn env e1 e2 e3)
  (match (interp defn env e1)
    [#f (interp defn env e3)]
    [_  (interp defn env e2)]))

;; store :: Env -> Symbol -> Val -> Env 
(define (store env x val)
  (cons (list x val) env))

;; lookup :: Env -> Symbol -> Val
(define (lookup env x)
  (match env
    ['() (raise (Err (string-append "Unbound identifier: " (symbol->string x))))]
    [(cons (list y val) rest) (if (eq? x y) val
                                  (lookup rest x))]))

(module+ test
  (check-equal? (interp-err (parse-prog '((+ 42 (sub1 34))))) 75)
  (check-equal? (interp-err (parse-prog '((zero? (- 5 (sub1 6)))))) #t)
  (check-equal? (interp-err (parse-prog '((if (zero? 0) (add1 5) (sub1 5))))) 6)
  (check-equal? (interp-err (parse-prog '((add1 (+ 3 #f)))))
                (Err "+ requires int"))
  (check-equal? (interp-err (parse-prog '((add1 (and #t #t)))))
                (Err "add1 expects int"))
  (check-equal? (interp-err (parse-prog '((/ 5 (sub1 1)))))
                (Err "division by 0 not allowed"))
  (check-equal? (interp-err (parse-prog '((let ((x 1)) (+ x 3))))) 4)
  (check-equal? (interp-err (parse-prog '((let ((x 1))
                                      (let ((y 2))
                                        (+ x y)))))) 3)
  (check-equal? (interp-err (parse-prog '((let ((x (add1 6)))
                                      (let ((x (+ 6 x)))
                                        (/ x 2)))))) 6)
  (check-equal? (interp-err (parse-prog '((let ((x (add1 6)))
                                      (let ((x (+ 6 x)))
                                        (/ x y))))))
                (Err "Unbound identifier: y"))

  (check-equal? (interp-err (parse-prog '((define (abs x)
                                            (if (<= x 0) (* -1 x) x))

                                          (abs -42)))) 42)

  (check-equal? (interp-err (parse-prog '((define (odd? x)
                                            (if (zero? x) #f
                                                (even? (sub1 x))))

                                          (define (even? x)
                                            (if (zero? x) #t
                                                (odd? (sub1 x))))

                                          (odd? 45)))) #t)

  (check-equal? (interp-err (parse-prog '((foo 4))))
                (Err "Definition not found: foo")))
