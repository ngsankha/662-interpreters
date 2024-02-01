#lang racket

(provide Int UnOp BinOp)

;; type Expr =
;; | (Int Integer)
(struct Int (i) #:prefab)
