open OUnit2
open Utils

let key_suite = "key" >::: List.map (fun (k, f) ->
  "simple" >:: (fun _ -> assert_set k (Atom.key f))) Lset.([
    (sing "x", f1);
    (sing "z", f2);
    (empty, f3);
    (empty ++ "x" ++ "y" ++ "z", g);
  ])

let vars_suite = "vars" >::: List.map (fun (vars, f) ->
  "simple" >:: (fun _ -> assert_set vars (Atom.vars f))) Lset.([
    (pair "x" "y", f1);
    (sing "z", f2);
    (sing "x", f3);
    (empty ++ "x" ++ "y" ++ "z", g);
  ])

let substitute_suite = "substitute" >::: [
  "incomplete" >:: (fun _ ->
    let atom = Atom.substitute theta3 g in
    assert_equal atom.Atom.relation.RN.name g.Atom.relation.RN.name;
    assert_equal x (atom.Atom.f "B");
    assert_equal a (atom.Atom.f "C");
    assert_equal b (atom.Atom.f "D");
  );
  "cyclic" >:: (fun _ ->
    let theta = Substitution.(id + "y" * x + "x" * y) in
    let atom = Atom.substitute theta f1 in
    assert_equal y (atom.Atom.f "A");
    assert_equal x (atom.Atom.f "B");
  );
]

let is_ground_suite = "is_ground" >::: [
  "positive" >:: (fun _ ->
    let atom = Atom.({
      relation = RN.binary "R" "A" "B";
      f = function "A" -> a | "B" | _ -> b
    }) in
    assert_true (Atom.is_ground atom)
  );
  "incomplete substitution" >:: (fun _ ->
    let theta = Substitution.(id + "x" * a) in
    let atom = Atom.substitute theta f1 in
    assert_false (Atom.is_ground atom)
  );
]

let equality_suite = "=" >::: [
  "recreation" >:: (fun _ ->
    let atom = Atom.({ relation = rs; f = fun _ -> z }) in
    assert_true Atom.(f2 = atom)
  );
  "identity substitution" >:: (fun _ ->
    let atom = Atom.substitute Substitution.id g in
    assert_true Atom.(g = atom)
  );
  "double substitution" >:: (fun _ ->
    let theta = Substitution.(id + "y" * z) in
    let atom = Atom.substitute theta f1 in
    assert_false Atom.(f2 = atom);
    let theta' = Substitution.(id + "x" * z) in
    let atom' = Atom.substitute theta' atom in
    assert_true Atom.(f2 = atom');
  );
]

let suite = "Atom tests" >::: [
  key_suite;
  vars_suite;
  substitute_suite;
  is_ground_suite;
  equality_suite;
]
