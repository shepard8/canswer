type constant = string
type variable = string
type t =
  | Constant of constant
  | Variable of variable

let vars l =
  Lset.mapfilter (function Variable v -> Some v | _ -> None) l

let var x = Variable x
let con x = Constant x

