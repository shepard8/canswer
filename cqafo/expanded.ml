type id = int
type node = {
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

let subqueries ag cqafo =
  let eq = CqaFO.( = ) in
  let rec aux acc vars = match Lset.pop vars with
  | None -> acc
  | Some (v, vars) -> aux Lset.(acc + map ~eq (CqaFO.substitute cqafo v) vars) vars
  in
  let substs_in = aux (Lset.empty_eq eq) cqafo.CqaFO.insourced in
  let substs_out = aux (Lset.empty_eq eq) cqafo.CqaFO.outsourced in
  let attvars = Lset.mapmerge (fun a -> a.AttackGraph.attvars) ag in
  let unattvars = Lset.(cqafo.CqaFO.insourced - attvars) in
  let outsourced_strict = Lset.map ~eq (CqaFO.outsource cqafo) attvars in
  let outsourced_equiv = Lset.map ~eq (CqaFO.outsource cqafo) unattvars in
  (Lset.(substs_in + substs_out + outsourced_strict), outsourced_equiv)

let counter = ref 0
let eq_node n n' = n.id = n'.id

let make q =
  let rec aux equiv parent acc cqafo =
    match Lset.pop (Lset.filter (fun n -> CqaFO.(n.cqafo = cqafo)) acc.nodes) with
    | Some (node, _) ->
        let edge = { source = parent; dest = node.id; equiv } in
        { acc with edges = Lset.(edge +< acc.edges) }
    | None ->
        let id = !counter in
        counter := id + 1;
        let ag = AttackGraph.make (CqaFO.freed_query cqafo) in
        let nodes = Lset.({ id; cqafo; ag } +< acc.nodes) in
        let edges = Lset.({ source = parent; dest = id; equiv } +< acc.edges) in
        let sub_strict, sub_equiv = subqueries ag cqafo in
        let acc = Lset.fold_left (aux true id) { nodes; edges } sub_equiv in
        Lset.fold_left (aux false id) acc sub_strict
  in
  let cqafo = CqaFO.make q in
  let start = { nodes = Lset.empty_eq eq_node; edges = Lset.empty } in
  let g = aux true !counter start cqafo in
  { g with edges = Lset.filter (fun e -> e.source <> e.dest) g.edges }

