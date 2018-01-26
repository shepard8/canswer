open OUnit2
open Utils

let symbol_suite = "symbol" >::: List.map (fun (x, s) ->
  "simple" >:: (fun _ -> assert_equal x (FromString.symbol s)))
[
  (x, "x");
  (y, "y");
  (a, "a");
  (b, "b");
  (Symbol.Constant "constant", "constant");
  (Symbol.Variable "variable", "variable");
  (Symbol.Constant "c42", "c42");
  (Symbol.Variable "x42", "x42");
]

let relation_name_suite = "relation_name" >::: List.map (fun (rn, s) ->
  "simple" >:: (fun _ ->
    assert_true RelationName.(rn = FromString.relation_name s)))
[
  (rs, "R(A | B)");
  (ss, "S(B C D|)");
  (ts, " T  (   |    A ) ");
  (RN.binary "Relation" "Attr1" "Attr2", "Relation(Attr1|Attr2)");
]

let substitution_suite = "substitution" >::: List.map (fun (theta, s) ->
  "simple" >:: (fun _ ->
    assert_true Substitution.(theta = FromString.substitution s)))
[
  (theta1, "x->y y->a z->x");
  (theta2, " x -> z z -> x ");
  (theta3, "y->a       z->b");
  (Substitution.id, "x->x");
]

let atom_suite = "atom" >::: List.map (fun (atom, s) ->
  "simple" >:: (fun _ -> assert_true Atom.(atom = FromString.atom s)))
[
  (f1, "R( A:x | B:y )");
  (f2, " R ( A : z | B : z ) ");
  (f3, "R(A:a|B:x)");
  (g, "S(B:x C:y D:z | )");
]

let conjunctive_suite = "conjunctive" >::: List.map (fun (q, s) ->
  "simple" >:: (fun _ ->
    assert_true Conjunctive.(q = FromString.conjunctive s)))
[
  (q1, " R ( A : x | B : y ) R ( A : z | B : z ) R ( A : a | B : x ) ");
  (q2, "x | R(A:x | B:y) R(A:z | B:z) R(A:a | B:x)");
  (q3, "R(A:x|B:y)S(B:x C:y D:z|)");
  (C.(empty + f1), "R(A:x|B:y)");
]

let suite = "Parser tests" >::: [
  symbol_suite;
  relation_name_suite;
  substitution_suite;
  atom_suite;
  conjunctive_suite;
]
