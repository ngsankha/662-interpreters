#lang racket

(provide Val UnOp BinOp If Err Err? Let Var App Defn Prog)

;; type Expr =
;; | (Val v)
;; | (UnOp u e)
;; | (BinOp b e e)
;; | (If e e e)
(struct Val (v) #:prefab)
(struct Var (x) #:prefab)
(struct UnOp (u e) #:prefab)
(struct BinOp (b e1 e2) #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Let (x e1 e2) #:prefab)

(struct App (f args) #:prefab)
(struct Defn (f args e) #:prefab)
(struct Prog (defns e) #:prefab)

(struct Err (err) #:prefab)
