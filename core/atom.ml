module RN = RelationName

type t = {
  relation : RelationName.t;
  f : RelationName.attribute -> Symbol.t;
}

let substitute t atom =
  { atom with f = fun attr -> Substitution.apply t (atom.f attr) }

let key atom =
  let s = Lset.filter atom.relation.RN.key atom.relation.RN.attributes in
  let s = Lset.map atom.f s in
  Symbol.vars s

let vars atom =
  let s = Lset.map atom.f atom.relation.RN.attributes in
  Symbol.vars s

let is_ground atom = Lset.(vars atom = empty)

let ( = ) atom1 atom2 =
  let r = atom1.relation in
  RN.(r = atom2.relation) &&
  Lset.for_all (fun attr -> atom1.f attr = atom2.f attr) r.RN.attributes
