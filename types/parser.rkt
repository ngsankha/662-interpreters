#lang racket

(require "ast.rkt")

(provide parse)

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?)                   (Val s)]
    [(? boolean?)                   (Val s)]
    [(? symbol?)                    (Var s)]
    [(list (? unop? u) e)           (UnOp u (parse e))]
    [(list (? binop? b) e1 e2)      (BinOp b (parse e1) (parse e2))]
    [`(if ,e1 ,e2 ,e3)              (If (parse e1) (parse e2) (parse e3))]
    [`(let ((,x : ,ty ,e1)) ,e2)    (Let x (parse-type ty) (parse e1) (parse e2))]
    [`(lambda (,@xts) : ,t ,e)      (Lam (parse-xts xts) (parse-type t) (parse e))]
    [`(λ      (,@xts) : ,t ,e)      (Lam (parse-xts xts) (parse-type t) (parse e))]
    [`(list ,@es)                   (List (map parse es))]
    [`(map ,e1 ,e2)                 (Map (parse e1) (parse e2))]
    [(cons e es)                    (App (parse e) (map parse es))]
    [_                              (error "Parse error!")]))

(define (parse-xts xts)
  (match xts
    ['()               '()]
    [`(,x : ,t)        (cons (list x (parse-type t)) '())]
    [`(,x : ,t ,@rest) (cons (list x (parse-type t)) (parse-xts rest))]))

(define (parse-type t)
  (match t
    ['int            (T 'int)]
    ['bool           (T 'bool)]
    ['list           (T 'list)]
    [`(U ,t1 ,t2)    (UnionT (parse-type t1) (parse-type t2))]
    [`(-> ,@ts ,ret) (FnT (map parse-type ts) (parse-type ret))]
    [`(,bt ,pt)      (ParamT (parse-type bt) (parse-type pt))]
    [_               (error "Unhandled type")]))

;; Any -> Boolean
(define (unop? x)
  (memq x '(add1 sub1 zero?)))

;; Any -> Boolean
(define (binop? x)
  (memq x '(+ - * / <= and)))
