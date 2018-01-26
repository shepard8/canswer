open Printf
module RN = RelationName
module AG = AttackGraph
module C = Conjunctive
module S = Substitution

type dot = string

let symbol = function
  | Symbol.Constant a -> sprintf "<b>%s</b>" a
  | Symbol.Variable x -> x

let symbols atom =
  let r = atom.Atom.relation in
  let f a = symbol (atom.Atom.f a) in
  let key = Lset.mapconcat " " f (RN.key r) in
  let other = Lset.mapconcat " " f Lset.(r.RN.attributes - RN.key r) in
  if key = "" then "(" ^ other ^ ")"
  else sprintf "(<u>%s</u>&#160;%s)" key other

let atom f =
  f.Atom.relation.RelationName.name ^ symbols f

let node ?(unattacked=false) n =
  let reference = atom n.AG.atom in
  let keycl =
    "<font point-size=\"10\">{" ^
    String.concat ", " n.AG.keycl.Lset.l ^
    "}</font><br />"
  in
  let f = atom n.AG.atom in
  let attvars =
    "<br /><font point-size=\"10\">{" ^
    String.concat ", " n.AG.attvars.Lset.l ^
    "}</font>"
  in
  let hl = if unattacked then " style=\"filled\" fillcolor=\"#FFCCBB\"" else "" in
  sprintf "\"%s\" [label=<%s%s%s>%s];" reference keycl f attvars hl

let attack_graph ag =
  let unatt = AG.unattacked ag in
  let node n = node ~unattacked:(Lset.mem n.AG.atom unatt) n in
  let nodes = Lset.mapconcat "\n" node ag in
  let node_attacks node =
    let attacker = atom node.AG.atom in
    Lset.map (fun f ->
      let attacked = atom f in
      let s = if AG.strong ag node f then "penwidth=2," else "" in
      let t = if AG.transitive ag node f then "style=dashed," else "" in
      let c = if AG.cyclic ag node f then "color=red," else "" in
      sprintf "\"%s\" -> \"%s\" [%s%s%s];" attacker attacked s t c
    ) node.AG.attatoms
  in
  let attacks = Lset.concat "\n" (Lset.mapmerge node_attacks ag) in
  sprintf "%s\n%s" nodes attacks

let split_attack_graph q =
  let parts = Conjunctive.split q in
  if Lset.size parts < 2 then
    sprintf "digraph {\n%s}" (attack_graph (AttackGraph.make q))
  else
    let c = ref 1 in
    let cluster q =
      let i = !c in
      c := !c + 1;
      let ag = attack_graph (AttackGraph.make q) in
      sprintf "subgraph cluster_%i {\n%s\nlabel=\"Subquery %i\"}" i ag i
    in
    let clusters = Lset.map cluster (Conjunctive.split q) in
    sprintf "digraph {\n%s}" (Lset.concat "\n" clusters)

let cqafo { CqaFO.query; CqaFO.outsourced; CqaFO.insourced } =
  let free_f s v = S.(s + v * Symbol.var ("<o>" ^ v ^ "</o>")) in
  let outsourced_f s v = S.(s + v * Symbol.var (String.uppercase v)) in
  let insourced_f s v = S.(s + v * Symbol.var (String.lowercase v)) in
  let subst = Lset.fold_left free_f S.id (Symbol.vars query.C.free) in
  let subst = Lset.fold_left outsourced_f subst outsourced in
  let subst = Lset.fold_left insourced_f subst insourced in
  let query = Conjunctive.substitute subst query in
  Lset.mapconcat " " atom query.C.atoms

let latedge e =
  let suffix = if e.Expanded.equiv then " [penwidth=3]" else "" in
  sprintf "%i -> %i%s;" e.Expanded.source e.Expanded.dest suffix

let latnode n =
  let color = if AttackGraph.has_cycle n.Expanded.ag then "red" else "green" in
  sprintf "%i [label=<%s>; style=filled; fillcolor=%s];\n"
  n.Expanded.id (cqafo n.Expanded.cqafo) color

let lattice l =
  "digraph {\n" ^
  Lset.mapconcat "\n" latnode l.Expanded.nodes ^
  "\n\n" ^
  Lset.mapconcat "\n" latedge l.Expanded.edges ^
  "\n}"

