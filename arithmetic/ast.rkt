#lang racket

(provide Int UnOp BinOp)

;; type Expr =
;; | (Int Integer)
;; | (UnOp u e)
;; | (BinOp b e e)
(struct Int (i) #:prefab)
(struct UnOp (u e) #:prefab)
(struct BinOp (b e1 e2) #:prefab)
