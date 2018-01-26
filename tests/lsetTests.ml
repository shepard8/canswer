open OUnit2
open Utils

let assert_listset l s = assert_bool "Unequal sets" Lset.(from_list l = s)

let empty_suite = "empty" >::: [
  "empty" >:: (fun _ -> assert_equal [] Lset.empty.Lset.l);
  "empty_eq" >::
    (fun _ -> assert_equal [] (Lset.empty_eq (fun _ _ -> false)).Lset.l);
  "empty_eq_f" >::
    (fun _ ->
      let s = Lset.empty_eq pmod10 in
      assert_true (s.Lset.eq 5 15));
]

let plusplus_suite = "++" >::: [
  "commutative" >::
    (fun _ -> assert_set Lset.(empty ++ 1 ++ 2) Lset.(empty ++ 2 ++ 1));
  "idempotent" >::
    (fun _ -> assert_set Lset.(empty ++ 1 ++ 1) Lset.(empty ++ 1));
  "mod10" >::
    (fun _ -> assert_set Lset.(smod10 ++ 2 ++ 12) Lset.(smod10 ++ 2));
]

let init_suite = "init" >::: [
  "nonnegative" >:: (fun _ -> assert_raises (Invalid_argument "Negative arity")
    (fun _ -> Lset.init (fun _ -> true) (-5))
  );
  "simple" >::
    (fun _ -> assert_listset [true] Lset.(init (fun _ -> true) 5));
  "simple2" >::
    (fun _ -> assert_listset [0;1;2;3] Lset.(init (fun i -> i) 4));
  "simple3" >::
    (fun _ -> assert_listset [true;false] Lset.(init (fun i -> i mod 2 < 1) 5));
  "mod10" >::
    (fun _ -> assert_listset [0] Lset.(init ~eq:pmod10 (fun i -> i * 10) 5));
]

let sing_suite = "sing" >::: [
  "simple" >:: (fun _ -> assert_listset [5] (Lset.sing 5));
]

let pair_suite = "pair" >::: [
  "simple" >:: (fun _ -> assert_listset [1;2] (Lset.pair 1 2));
  "commutative" >:: (fun _ -> assert_set (Lset.pair 1 2) (Lset.pair 2 1));
  "sing" >:: (fun _ -> assert_listset [5] (Lset.pair 5 5));
  "mod10" >:: (fun _ -> assert_listset [5] (Lset.pair ~eq:pmod10 5 15));
]

let size_suite = "size" >::: List.map (fun (n, s) ->
  "simple" >:: (fun _ -> assert_equal n (Lset.size s))) Lset.([
    (2, pair 1 2);
    (1, pair 1 1);
    (1, sing 2);
    (3, empty ++ 1 ++ 2 ++ 3);
    (2, empty ++ 1 ++ 2 ++ 1);
    (5, init (fun i -> i) 5);
    (1, init (fun i -> 0) 5);
    (0, init (fun i -> 1) 5 - init (fun i -> i) 5);
    (5, init (fun i -> i) 3 + init (fun i -> i) 5);
    (5, init (fun i -> i) 3 ++ 42 ++ 21);
    (5, from_list [1; 2; 3; 21; 42]);
    (2, smod10 ++ 5 ++ 10 ++ 15 ++ 20);
  ])

let subset_suite = "<=" >::: List.map (fun (s, t) ->
  "simple" >:: (fun _ -> assert_bool "<= failed" Lset.(s <= t))) Lset.([
    (sing 5, pair 5 6);
    (pair 5 5, sing 5);
    (empty ++ 1 ++ 2 ++ 3, init (fun i -> i) 5);
    (from_list [1;2;1], empty ++ 1 ++ 2 ++ 3);
    (smod10 ++ 5 ++ 15, smod10 ++ 5);
  ])

let equal_suite = "=" >::: List.map (fun (s, t) ->
  "simple" >:: (fun _ -> assert_bool "= failed" Lset.(s = t))) Lset.([
    (sing 5, sing 5);
    (sing 5, pair 5 5);
    (sing 5, empty ++ 5 ++ 5 ++ 5);
    (sing 5, init (fun _ -> 5) 10);
    (sing 5, from_list [5; 5; 5]);
    (init (fun i -> i) 5 ++ 5, init (fun i -> succ i) 5 ++ 0);
    (smod10 ++ 5 ++ 15, smod10 ++ 5);
  ])

let union_suite = "+" >::: List.map (fun (l, s) ->
  "simple" >:: (fun _ -> assert_bool "+ failed" Lset.(from_list l = s))) Lset.([
    ([5], empty + empty ++ 5);
    ([1;2;3], sing 1 + pair 2 3);
  ])

let inter_suite = "^" >::: List.map (fun (l, s) ->
  "simple" >:: (fun _ -> assert_bool "^ failed" Lset.(from_list l = s))) Lset.([
    ([5], init (fun i -> succ i) 5 ^ init (fun i -> i * 5) 5);
    ([3], pair 2 3 ^ pair 3 4);
    ([2], pair 2 3 ^ pair 4 2);
  ])

let diff_suite = "-" >::: List.map (fun (l, s) ->
  "simple" >:: (fun _ -> assert_bool "- failed" Lset.(from_list l = s))) Lset.([
    ([5], init (fun i -> succ i) 5 - init (fun i -> succ i) 4);
    ([1;2], pair 1 2 - pair 0 3);
    ([1], pair 1 2 + pair 2 3 - sing 2 - sing 3);
  ])

let mapfilter_suite = "mapfilter" >::: [
  "simple" >:: (fun _ ->
    let f x = if x mod 2 = 0 then Some (x * 5) else None in
    let s = Lset.mapfilter f smod10full in
    assert_listset [0; 10; 20; 30; 40] s
  );
  "with_eq" >:: (fun _ ->
    let f x = if x mod 2 = 0 then Some (x * 5) else None in
    let s = Lset.mapfilter ~eq:pmod10 f smod10full in
    match Lset.pop s with
    | None -> assert_failure "Set s should not be empty"
    | Some (x, s) ->
        assert_listset [] s;
        assert_equal 0 (x mod 10)
  );
]

let mapmerge_suite = "mapmerge" >::: [
  "simple" >:: (fun _ ->
    let f n = Lset.init (fun i -> i) n in
    let s = Lset.mapmerge f (Lset.pair 5 7) in
    assert_set (f 7) s
  );
]

let suite = "Tools tests" >::: [
  empty_suite;
  plusplus_suite;
  init_suite;
  sing_suite;
  pair_suite;
  size_suite;
  subset_suite;
  equal_suite;
  union_suite;
  inter_suite;
  diff_suite;
  mapfilter_suite;
  mapmerge_suite;
]
