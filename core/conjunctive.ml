type t = {
  atoms : Atom.t Lset.t;
  free : Symbol.t Lset.t;
}

let vars q = Lset.(mapmerge Atom.vars q.atoms - Symbol.vars q.free)

let empty = { atoms = Lset.empty_eq Atom.( = ); free = Lset.empty }

let ( @ ) s q =
  match s with
  | Symbol.Variable v when Lset.mem v (vars q) ->
      { q with free = Lset.(q.free +> s) }
  | Symbol.Constant _ -> { q with free = Lset.(q.free +> s) }
  | Symbol.Variable _ -> q

let ( + ) q f =
  { q with atoms = Lset.(f +< q.atoms) }

let ( - ) q f =
  { q with atoms = Lset.(q.atoms - sing ~eq:Atom.( = ) f) }

let ( -- ) q f =
  let atoms = Lset.(q.atoms - sing ~eq:Atom.( = ) f) in
  let tofree = Lset.(q.free + map (fun x -> Symbol.Variable x) (Atom.vars f)) in
  Lset.fold_right ( @ ) tofree (Lset.fold_left ( + ) empty atoms)

let substitute theta q =
  let atoms = Lset.(map ~eq:Atom.( = ) (Atom.substitute theta) q.atoms) in
  let free = Lset.(map (Substitution.apply theta) q.free) in
  Lset.fold_right ( @ ) free (Lset.fold_left ( + ) empty atoms)

let self_join_free q =
  let rn = Lset.map ~eq:RelationName.( = ) (fun a -> a.Atom.relation) q.atoms in
  Lset.size rn = Lset.size q.atoms

let ( = ) q q' =
  Lset.(q.free = q'.free && q.atoms = q'.atoms)

let atomvars q f =
  Lset.(Atom.vars f - Symbol.vars q.free)

let atomkey q f =
  Lset.(Atom.key f - Symbol.vars q.free)

let merge q r =
  Lset.fold_right ( @ ) r.free (Lset.fold_left ( + ) q r.atoms)

let split q =
  let related q r = not Lset.(vars q ^ vars r = empty) in
  let s = Lset.map ~eq:( = ) (fun atom -> (empty + atom)) q.atoms in
  let s = Lset.map ~eq:( = ) (fun aq -> Lset.fold_right ( @ ) q.free aq) s in
  Lset.merge related merge s

