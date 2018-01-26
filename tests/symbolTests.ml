open OUnit2
open Utils

let vars_suite = "vars" >::: List.map (fun (s, t) ->
  "simple" >:: (fun _ -> assert_set s (Symbol.vars t))) Lset.([
  (pair "x" "y", empty ++ a ++ x ++ a ++ x ++ y);
  (sing "x", empty ++ a ++ b ++ x);
  (empty, empty ++ a ++ b ++ a);
  (pair "x" "y" + sing "z", empty ++ x ++ y ++ z);
])

let suite = "Symbol tests" >::: [
  vars_suite;
]
