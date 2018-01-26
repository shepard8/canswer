open OUnit2

let suite = "Canswer" >::: [
  LsetTests.suite;
  SymbolTests.suite;
  RelationNameTests.suite;
  SubstitutionTests.suite;
  AtomTests.suite;
  ConjunctiveTests.suite;
  FromStringTests.suite;
]

let () =
  OUnit2.run_test_tt_main suite
