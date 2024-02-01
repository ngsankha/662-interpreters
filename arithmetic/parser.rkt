#lang racket

(require "ast.rkt")

(provide parse)

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [(list (? unop? u) e) (UnOp u (parse e))]
    [(list (? binop? b) e1 e2) (BinOp b (parse e1) (parse e2))]
    [_ (error "Parse error!")]))

;; Any -> Boolean
(define (unop? x)
  (memq x '(add1 sub1)))

;; Any -> Boolean
(define (binop? x)
  (memq x '(+ - * /)))

