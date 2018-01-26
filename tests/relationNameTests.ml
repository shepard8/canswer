open OUnit2
open Utils
module R = RelationName

let signature_test = "signature" >:: fun _ ->
  let r = R.signature "R" 5 2 in
  assert_equal "R" r.R.name;
  assert_equal 5 (Lset.size r.R.attributes);
  assert_equal 2 Lset.(size (filter r.R.key r.R.attributes))

let fullkey_test = "fullkey" >:: fun _ ->
  let r = R.fullkey "S" (Lset.pair "A" "B") in
  assert_equal "S" r.R.name;
  assert_set (Lset.pair "A" "B") r.R.attributes;
  assert_set (Lset.pair "A" "B") (Lset.filter r.R.key r.R.attributes)

let binary_test = "binary" >:: fun _ ->
  let r = R.binary "T" "X" "Y" in
  assert_equal "T" r.R.name;
  assert_set (Lset.pair "X" "Y") r.R.attributes;
  assert_set (Lset.sing "X") (Lset.filter r.R.key r.R.attributes)

let suite = "Symbol tests" >::: [
  signature_test;
  fullkey_test;
  binary_test;
]
