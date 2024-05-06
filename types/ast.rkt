#lang racket

(provide Val UnOp BinOp If Err Err?
         Let Var App Lam List Map
         T UnionT FnT ParamT)

; type Values :=
;   | (Val v)
;   | (Lam xs e)
(struct Val   (v)       #:prefab)
(struct Lam   (xts t e) #:prefab)

; type Expr :=
;   | Values
;   | (Var   x)
;   | (UnOp  u e)
;   | (BinOp u e)
;   | (If    e e e)
;   | (Let   x e e)
;   | (App   e e)
;   | (List  es)
(struct Var    (x)         #:prefab)
(struct UnOp   (u e)       #:prefab)
(struct BinOp  (b e1 e2)   #:prefab)
(struct If     (e1 e2 e3)  #:prefab)
(struct Let    (x t e1 e2) #:prefab)
(struct App    (x args)    #:prefab)
(struct List   (es)        #:prefab)
(struct Map    (e1 e2)     #:prefab)

(struct Err   (err)       #:prefab)

(struct T (t)          #:prefab) ; int | bool
(struct UnionT (t1 t2) #:prefab) ; T U T
(struct ParamT (b p)   #:prefab) ; B<P>
(struct FnT (args ret) #:prefab) ; T -> T
