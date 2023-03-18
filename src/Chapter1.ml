type expression =
  | Var of string
  | Const of int
  | Add of expression * expression
  | Mult of expression * expression

let simplify1 expr =
  match expr with
  | Add (Const m, Const n) -> Const (m + n)
  | Mult (Const m, Const n) -> Const (m * n)
  | Add (Const 0, x) -> x
  | Add (x, Const 0) -> x
  | Mult (Const 0, _) -> Const 0
  | Mult (x, Const 0) -> Const 0
  | Mult (Const 1, x) -> x
  | Mult (x, Const 1) -> x
  | _ -> expr

let rec simplify expr =
  match expr with
  | Add (e1, e2) -> simplify1 (Add (simplify e1, simplify e2))
  | Mult (e1, e2) -> simplify1 (Mult (simplify e1, simplify e2))
  | _ -> simplify1 expr
