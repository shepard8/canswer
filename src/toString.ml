let symbol = function
  | Symbol.Constant a -> a
  | Symbol.Variable x -> x

let relation_name relation =
  let keyattrs = RelationName.key relation in
  let nonkeyattrs = Lset.(relation.RelationName.attributes - keyattrs) in
  let keystr = Lset.concat " " keyattrs in
  let nonkeystr = Lset.concat " " nonkeyattrs in
  Printf.sprintf "%s(%s | %s)" relation.RelationName.name keystr nonkeystr

let substitution theta =
  let f x =
    let theta_x = Substitution.apply theta (Symbol.Variable x) in
    Printf.sprintf "%s->%s" x (symbol theta_x)
  in
  Lset.mapconcat " " f (Substitution.domain theta)

let format_map f a = match a.[0] with
| '0' .. '9' -> symbol (f.Atom.f a)
| _ -> a ^ ":" ^ symbol (f.Atom.f a)

let atom atom =
  let relation = atom.Atom.relation in
  let keyattrs = RelationName.key relation in
  let nonkeyattrs = Lset.(relation.RelationName.attributes - keyattrs) in
  let keystr = Lset.mapconcat " " (format_map atom) keyattrs in
  let nonkeystr = Lset.mapconcat " " (format_map atom) nonkeyattrs in
  Printf.sprintf "%s(%s|%s)" relation.RelationName.name keystr nonkeystr

let conjunctive q =
  let freestr = Lset.mapconcat " " symbol q.Conjunctive.free in
  let atomsstr = Lset.mapconcat " " atom q.Conjunctive.atoms in
  if freestr = "" then atomsstr else Printf.sprintf "%s | %s" freestr atomsstr

let rec drcf =
  let unary b f a =
    b ^ drcf f ^ a in
  let binary f c g =
    "(" ^ drcf f ^ c ^ drcf g ^ ")" in
  let nary c s =
    "(" ^ Lset.mapconcat c drcf s ^ ")" in
  let quant q vs f =
    q ^ " " ^ Lset.concat " " vs ^ " " ^ drcf f in
  function
    | Drc.Certain q -> unary "[" q "]"
    | Drc.True -> "true"
    | Drc.False -> "false"
    | Drc.Atom f -> atom f
    | Drc.And s -> nary " /\\ " s
    | Drc.Or s -> nary " \\/ " s
    | Drc.Neg f -> unary "~ " f ""
    | Drc.Implies (f, g) -> binary f " -> " g
    | Drc.Equiv (f, g) -> binary f " <-> " g
    | Drc.Exists (vs, f) -> quant "exists" vs f
    | Drc.Forall (vs, f) -> quant "forall" vs f

let drc {Drc.free; Drc.formula} =
  "{ " ^ Lset.mapconcat ", " symbol free ^ " | " ^ drcf formula ^ " }"

let trcvar (v, r) = v ^ "_" ^ r.RelationName.name
let trcsymbol = function
  | Trc.Map (v, a) -> trcvar v ^ "." ^ a
  | Trc.Constant c -> c
let trcatom (a, b) = trcsymbol a ^ " = " ^ trcsymbol b

let rec trcf =
  let unary b f a =
    b ^ trcf f ^ a in
  let binary f c g =
    "(" ^ trcf f ^ c ^ trcf g ^ ")" in
  let nary c s =
    "(" ^ Lset.mapconcat c trcf s ^ ")" in
  let quant q vs f =
    q ^ " " ^ Lset.mapconcat " " trcvar vs ^  " " ^ trcf f in
  function
    | Trc.Certain q -> unary "[" q "]"
    | Trc.True -> "true"
    | Trc.False -> "false"
    | Trc.Atom f -> trcatom f
    | Trc.And s -> nary " /\\ " s
    | Trc.Or s -> nary " \\/ " s
    | Trc.Neg f -> unary "~ " f ""
    | Trc.Implies (f, g) -> binary f " -> " g
    | Trc.Equiv (f, g) -> binary f " <-> " g
    | Trc.Exists (vs, f) -> quant "exists" vs f
    | Trc.Forall (vs, f) -> quant "forall" vs f

let trc {Trc.free; Trc.formula} =
  "{ " ^ Lset.mapconcat ", " trcsymbol free ^ " | " ^ trcf formula ^ " }"

