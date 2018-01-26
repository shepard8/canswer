open OUnit2
open Utils

let make_suite = "@ +" >::: [
  "eq condition" >:: (fun _ ->
    let f = Atom.({relation = rs; f = function "A" -> x | _ -> y }) in
    let q = Conjunctive.(empty + f + f1) in
    assert_equal 1 (Lset.size q.Conjunctive.atoms)
  );
  "unbounded free variables" >:: (fun _ ->
    let q = Conjunctive.(x @ a @ empty + f2) in
    assert_equal 1 (Lset.size q.Conjunctive.free)
  );
  "empty query" >:: (fun _ ->
    let q = Conjunctive.empty in
    assert_equal 0 (Lset.size q.Conjunctive.atoms);
    assert_equal 0 (Lset.size q.Conjunctive.free)
  );
  "query with self-joins" >:: (fun _ ->
    try
      let q = Conjunctive.(empty + f1 + f2) in
      assert_equal 2 (Lset.size q.Conjunctive.atoms)
    with _ -> assert_failure "bli"
  );
]

let minus_suite = "-" >::: [
  "simple" >:: (fun _ ->
    let q = Conjunctive.(empty + f1 + f2 - f1) in
    assert_equal 1 (Lset.size q.Conjunctive.atoms)
  );
  "free variables" >:: (fun _ ->
    let q = Conjunctive.(x @ y @ empty + f1) in
    let q2 = Conjunctive.(q - f1) in
    assert_equal 0 (Lset.size q2.Conjunctive.free)
  );
]

let vars_suite = "vars" >::: [
  "empty" >:: (fun _ ->
    assert_set Lset.empty Conjunctive.(vars (x @ y @ empty))
  );
  "freed" >:: (fun _ ->
    assert_set Lset.empty Conjunctive.(vars (x @ y @ empty + f1))
  );
  "with constants" >:: (fun _ ->
    assert_set
      Lset.(empty ++ "x" ++ "y" ++ "z")
      Conjunctive.(vars (a @ b @ empty + f1 + f2 + f3))
  );
  "positive" >:: (fun _ ->
    assert_set (Lset.pair "x" "y") Conjunctive.(vars (empty + f1))
  );
]

let substitute_suite = "substitute" >::: [
  "over atoms" >:: (fun _ ->
    let q = Conjunctive.substitute theta1 q1 in
    assert_set (Lset.pair "x" "y") (Conjunctive.vars q)
  );
  "over free" >:: (fun _ ->
    let q = Conjunctive.substitute theta1 q2 in
    assert_set (Lset.sing y) q.Conjunctive.free
  );
  "over free 2" >:: (fun _ ->
    let q = Conjunctive.substitute S.(id + "x" * a) q1 in
    assert_set Lset.empty q.Conjunctive.free
  );
]

let self_join_free_suite = "self_join_free" >::: [
  "simple" >:: (fun _ ->
    assert_false (Conjunctive.self_join_free q1);
    assert_false (Conjunctive.self_join_free q2);
    assert_true (Conjunctive.self_join_free q3)
  );
]

let suite = "Substitution tests" >::: [
  make_suite;
  minus_suite;
  vars_suite;
  substitute_suite;
  self_join_free_suite;
]
