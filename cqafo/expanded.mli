type id = int
type node = private {
  id : id;
  cqafo : CqaFO.t;
  ag : AttackGraph.t;
}
type edge = {
  source : id;
  dest : id;
  equiv : bool;
}
type t = {
  nodes : node Lset.t;
  edges : edge Lset.t;
}

val make : Conjunctive.t -> t

val eq_node : node -> node -> bool

