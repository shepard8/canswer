open OUnit2
open Utils

open Substitution

let identity_suite = "identity" >::: [
  "variable map" >:: (fun _ -> assert_equal x (apply id x));
]

let augment_suite = "+ *" >::: [
  "simple" >:: (fun _ -> assert_equal a (apply (id + "x" * a) x));
  "not immediate" >:: (fun _ ->
    let t = id + "y" * x + "x" * a in
    assert_equal x (apply t y));
  "hidden" >:: (fun _ ->
    let t = id + "x" * y + "x" * z in
    assert_equal z (apply t x));
]

let apply_suite = "apply" >::: [
  "simple" >:: (fun _ -> assert_equal a (apply theta1 y));
  "outside domain" >:: (fun _ -> assert_equal y (apply theta2 y));
]

let is_valuation_suite = "is_valuation" >::: [
  "simple" >:: (fun _ -> assert_false (is_valuation theta1));
  "simple2" >:: (fun _ -> assert_false (is_valuation theta2));
  "simple3" >:: (fun _ -> assert_true (is_valuation theta3));
]

let theta = id + "x" * y
let phi = id + "x" * y + "y" * z
let psi = id + "x" * y + "y" * y
let equal_suite = "equal" >::: [
  "subleft" >:: (fun _ -> assert_false (theta = phi));
  "subright" >:: (fun _ -> assert_false (phi = theta));
  "positive" >:: (fun _ -> assert_true (theta = psi));
]

let suite = "Substitution tests" >::: [
  identity_suite;
  augment_suite;
  apply_suite;
  is_valuation_suite;
  equal_suite;
]
