module RN = RelationName
module C = Conjunctive
module AG = AttackGraph

type atom_attr = Atom.t * RN.attribute
let aa_eq (f, a) (f', a') = Atom.(f = f') && a = a'

module Variable = struct
  type t = Symbol.variable
  let ( = ) (s : t) (t : t) = s = t
end

include Rc.Rc (Atom) (Variable)

let eq = ( = )

type t = {
  free : Symbol.t Lset.t;
  formula : formula;
}

let conjunctive q =
  let atoms = Lset.map ~eq datom q.C.atoms in
  { free = q.C.free; formula = dexists (C.vars q) (dand atoms) }

let v (f, a) = f.Atom.relation.RN.name ^ "'_{" ^ a ^ "}"
let s (f, a) = Symbol.Variable (v (f, a))

let eqatom (f, a) = datom Atom.{
  relation = RN.binary "=" "1" "2";
  f = function
    | "1" -> f.Atom.f a
    | "2" | _ -> Symbol.Variable (v (f, a))
}

let rec cons_attrs free acc s = match Lset.pop s with
| None -> acc
| Some (f, s) ->
    let rec aux free acc s = match Lset.pop s with
    | None -> (free, acc)
    | Some (a, s) ->
        match f.Atom.f a with
        | Symbol.Variable x when not (Lset.mem x free) ->
            aux Lset.(x +< free) acc s
        | _ -> aux free Lset.((f, a) +< acc) s
    in
    let nk = Lset.(f.Atom.relation.RN.attributes - RN.key f.Atom.relation) in
    let (free, acc) = aux free acc nk in
    cons_attrs free acc s

let f' cattrs f = datom Atom.{
  relation = f.relation;
  f = fun a -> if Lset.mem (f, a) cattrs then s (f, a) else f.Atom.f a
}

let rew free atoms phi =
  let avars = Lset.mapmerge Atom.vars atoms in
  let akeyvars = Lset.mapmerge Atom.key atoms in
  let cattrs = cons_attrs Lset.(free + akeyvars) (Lset.empty_eq aa_eq) atoms in
  let eqatoms = Lset.map ~eq eqatom cattrs in
  Lset.(
    dexists (avars - free) (
      dand (map ~eq datom atoms +> (
        dforall (avars - akeyvars - free + map v cattrs) (
          dimplies (dand (map ~eq (f' cattrs) atoms)) (dand (eqatoms +> phi))
        )))))

let rec rewrite group split q =
  let parts = if split then C.split q else Lset.sing q in
  Lset.fold_left (fun acc q -> dand (Lset.pair acc (
    let atoms = AG.(unattacked (make q)) in
    match atoms.Lset.l with
    | [] -> dcertain (conjunctive q).formula
    | _ when group -> aux group split q atoms
    | atom :: _ -> aux group split q Lset.(sing atom)
  ))) dtrue parts
and aux group split q atoms =
  let subq = Lset.fold_left C.( -- ) q atoms in
  rew (Symbol.vars q.C.free) atoms (rewrite group split subq)

let rewrite ?(group=true) ?(split=true) q = {
  free = q.C.free;
  formula = rewrite group split q;
}

