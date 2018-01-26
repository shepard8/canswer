let symbol = function
  | Symbol.Constant a -> "\\cst{" ^ a ^ "}"
  | Symbol.Variable x -> "\\var{" ^ x ^ "}"

let atom atom =
  let relation = atom.Atom.relation in
  let keyattrs = RelationName.key relation in
  let nonkeyattrs = Lset.(relation.RelationName.attributes - keyattrs) in
  let f a = symbol (relation.RelationName.f a) in
  let key = "\\key{" ^ String.concat ", " (List.map f keyattrs) ^ "}" in
  let nonkey = String.concat ", " (List.map f nonkeyattrs) in
  let symbols = String.concat ", " [key; nonkey] in
  Printf.sprintf "\\relation{%s}(%s)" relation.RelationName.name symbols

let conjunctive q =
  let free = Lset.map (symbol ?con_quotes) q.Conjunctive.free in
  let freestr = String.concat ", " free.Lset.l in
  let atoms = Lset.map atom q.Conjunctive.atoms in
  let atomsstr = String.concat " \\wedge " atoms.Lset.l in
  Printf.sprintf "\{%s \mid %s\}" freestr atomsstr
