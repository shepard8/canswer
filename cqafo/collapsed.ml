type collapsed = Expanded.t

open Expanded

let collapse_edge edges edge =
  Lset.map (fun e -> {
    source = if e.source = edge.source then edge.dest else e.source;
    dest = if e.dest = edge.source then edge.dest else e.dest;
    equiv = e.equiv;
  }) edges


let collapse { nodes; edges } =
  let rec aux edges =
    match Lset.pop (Lset.filter (fun e -> e.equiv && e.source <> e.dest) edges) with
    | None ->
        let ids = Lset.mapmerge (fun e -> Lset.pair e.source e.dest) edges in
        let edges = Lset.filter (fun e -> e.source <> e.dest) edges in
        let nodes = Lset.filter (fun n -> Lset.mem n.id ids) nodes in
        { nodes; edges }
    | Some (edge, _) -> aux (collapse_edge edges edge)
  in aux edges

let remove_transitive_edges { nodes; edges } = {
  nodes;
  edges = Lset.filter (fun e -> Lset.for_all (fun n ->
    not (Lset.mem { source = e.source; dest = n.id; equiv = false } edges) ||
    not (Lset.mem { source = n.id; dest = e.dest; equiv = false } edges)
  ) nodes) edges
}

let make g = remove_transitive_edges (collapse g)

