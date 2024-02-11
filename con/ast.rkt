#lang racket

(provide Val UnOp BinOp If)

;; type Expr =
;; | (Val v)
;; | (UnOp u e)
;; | (BinOp b e e)
;; | (If e e e)
(struct Val (v) #:prefab)
(struct UnOp (u e) #:prefab)
(struct BinOp (b e1 e2) #:prefab)
(struct If (e1 e2 e3) #:prefab)
