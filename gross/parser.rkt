#lang racket

(require "ast.rkt")

(provide parse-prog)

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Val s)]
    [(? boolean?) (Val s)]
    [(? symbol?)  (Var s)]
    [(list (? unop? u) e) (UnOp u (parse e))]
    [(list (? binop? b) e1 e2) (BinOp b (parse e1) (parse e2))]
    [`(if ,e1 ,e2 ,e3) (If (parse e1) (parse e2) (parse e3))]
    [`(let ((,x ,e1)) ,e2) (Let x (parse e1) (parse e2))]
    [`(,(? symbol? f) ,@args) (App f (map parse args))]
    [_ (error "Parse error!")]))

;; S-Expr -> Defn
(define (parse-defn s)
  (match s
    [`(define (,(? symbol? f) ,@args) ,e) (Defn f args (parse e))]
    [_ (error "parse error!")]))

;; List S-Expr -> Prog
(define (parse-prog s)
  (match s
    [(cons e '()) (Prog '() (parse e))]
    [(cons defn rest) (match (parse-prog rest)
                        [(Prog d e) (Prog (cons (parse-defn defn) d) e)])]))

;; Any -> Boolean
(define (unop? x)
  (memq x '(add1 sub1 zero?)))

;; Any -> Boolean
(define (binop? x)
  (memq x '(+ - * / <= and)))

