#lang racket

(require "ast.rkt")

(provide parse)

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Int s)]
    [_ (error "Parse error!")]))
