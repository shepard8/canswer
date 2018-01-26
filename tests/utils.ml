open OUnit2

module L = Lset
module Sy = Symbol
module RN = RelationName
module S = Substitution
module A = Atom
module C = Conjunctive

let assert_true b = assert_bool "Not true" b
let assert_false b = assert_bool "Not false" (not b)

let assert_set s t = assert_bool "Unequal sets" L.(s = t)

(* Lsets examples *)
let pmod10 a b = a mod 10 = b mod 10
let smod10 = L.empty_eq pmod10
let smod10full = L.(smod10 ++ 0 ++ 1 ++ 2 ++ 3 ++ 4 ++ 5 ++ 6 ++ 7 ++ 8 ++ 9)
let smod10even = L.(smod10 ++ 0 ++ 2 ++ 4 ++ 6 ++ 8)

(* Symbols examples *)
let var x = Symbol.Variable x

let x = var "x"
let y = var "y"
let z = var "z"

let con a = Symbol.Constant a

let a = con "a"
let b = con "b"
let c = con "c"

(* Relation names examples *)
let rs = RN.binary "R" "A" "B"
let ss = RN.fullkey "S" L.(empty ++ "B" ++ "C" ++ "D")
let ts = RN.make "T" (L.sing "A") (fun _ -> false)

(* Substitutions examples *)
let theta1 = S.(id + "x" * y + "y" * a + "z" * x)
let theta2 = S.(id + "x" * z + "z" * x)
let theta3 = S.(id + "y" * a + "z" * b)

(* Atoms examples *)
let f1 = A.({
  relation = rs;
  f = function
    | "A" -> x
    | "B" | _ -> y
})
let f2 = A.({
  relation = rs;
  f = fun _ -> z
})
let f3 = A.({
  relation = rs;
  f = function
    | "A" -> a
    | "B" | _ -> x
})
let g = A.({
  relation = ss;
  f = function
    | "B" -> x
    | "C" -> y
    | "D" | _ -> z
})

(* Conjunctive queries examples *)
let q1 = C.(empty + f1 + f2 + f3)
let q2 = C.(x @ q1)
let q3 = C.(empty + f1 + g)







