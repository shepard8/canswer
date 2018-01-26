module RN = RelationName
module C = Conjunctive
module AG = AttackGraph
module A = Atom

module Variable = struct
  type t = Symbol.variable * RN.t
  let ( = ) s t = fst s = fst t && RN.(snd s = snd t)
end

type symbol =
  | Map of Variable.t * RN.attribute
  | Constant of Symbol.constant

let eq_s a b = match (a, b) with
| Map (v, a), Map (v', a') -> Variable.(v = v') && a = a'
| Constant c, Constant c' -> c = c'
| _ -> false

module Atom = struct
  type t = symbol * symbol
  let ( = ) (a, b) (a', b') = eq_s a a' && eq_s b b' || eq_s a b' && eq_s b a'
end

include Rc.Rc (Atom) (Variable)

let datom (e, e') =
  if eq_s e e' then dtrue else datom (e, e')

let dexists = dexistsstrict
let dforall = dforallstrict

let eq = ( = )

type t = {
  free : symbol Lset.t;
  formula : formula;
}

let var name atom = (name, atom.A.relation)

let default_corr = function
  | Symbol.Constant c -> Constant c
  | Symbol.Variable _ -> Constant ""

let rec make_corr ?(tvar="t") init atoms = match Lset.pop atoms with
| None -> init
| Some (atom, atoms) ->
    let rec aux f attrs = match Lset.pop attrs with
    | None -> make_corr f atoms
    | Some (a, attrs) ->
        match atom.A.f a with
        | Symbol.Constant c -> aux f attrs
        | v when f v <> Constant "" -> aux f attrs
        | v -> aux (fun v' ->
            if Pervasives.(v = v') then Map (var tvar atom, a) else f v'
        ) attrs
    in aux init atom.A.relation.RN.attributes

let conjunctive q =
  let corr = make_corr default_corr q.C.atoms in
  let atoms = Lset.mapmerge ~eq (fun atom ->
    Lset.map ~eq (fun attr ->
      datom (Map (var "t" atom, attr), corr (atom.A.f attr))
    ) atom.A.relation.RN.attributes
  ) q.C.atoms in
  let vars = Lset.map ~eq:Variable.( = ) (var "t") q.C.atoms in
  { free = Lset.map ~eq:eq_s corr q.C.free; formula = dexists vars (dand atoms) }

let rec rewrite group split corr q = (* TODO atoms set, not query *)
  let parts = if split then C.split q else Lset.sing q in
  Lset.fold_left (fun acc q -> dand (Lset.pair acc (
    let atoms = AG.(unattacked (make q)) in
    match Lset.pop atoms with
    | None -> dcertain (conjunctive q).formula
    | _ when group -> aux group split corr q atoms
    | Some (atom, _) -> aux group split corr q Lset.(sing atom)
  ))) dtrue parts
and aux group split corr q atoms =
  let evars = Lset.map ~eq:Variable.( = ) (var "e") atoms in
  let uvars = Lset.map ~eq:Variable.( = ) (var "u") atoms in
  let keyeqs = Lset.mapmerge ~eq (fun atom ->
    Lset.map ~eq (fun a ->
      datom (Map (var "e" atom, a), Map (var "u" atom, a))
    ) (RN.key atom.A.relation)
  ) atoms in
  let corr = make_corr ~tvar:"u" corr atoms in
  let eqs = Lset.mapmerge ~eq (fun atom ->
    Lset.map ~eq (fun a ->
      datom (Map (var "u" atom, a), corr (atom.A.f a))
    ) (atom.A.relation.RN.attributes)
  ) atoms in
  let subq = Lset.fold_left C.( -- ) q atoms in
  let phi = rewrite group split corr subq in
  dexists evars (dforall uvars (dimplies (dand keyeqs) (dand Lset.(eqs +> phi))))

let rewrite ?(group=true) ?(split=true) q =
  let corr = make_corr ~tvar:"f" default_corr q.C.atoms in
  let free = Lset.map ~eq:eq_s corr q.C.free in
  let corr = function
    | s when Lset.mem s q.C.free -> corr s
    | s -> default_corr s
  in { free; formula = rewrite group split corr q }

