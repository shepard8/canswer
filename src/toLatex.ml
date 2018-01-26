open Printf
module RN = RelationName

type t = string

let symbol = function
  | Symbol.Constant a -> "\\constant{" ^ a ^ "}"
  | Symbol.Variable x -> "\\variable{" ^ x ^ "}"

let relation_name r =
  let keystr = Lset.concat ", " (RN.key r) in
  let nonkeystr = Lset.concat ", " Lset.(r.RN.attributes - RN.key r) in
  sprintf "\\atom{%s}{%s}{%s}" r.RN.name keystr nonkeystr

let substitution theta =
  let f x =
    let theta_x = Substitution.apply theta (Symbol.Variable x) in
    sprintf "%s \\mapsto %s" x (symbol theta_x)
  in
  Lset.mapconcat ", " f (Substitution.domain theta)

let format_map f a = match a.[0] with
| '0' .. '9' -> symbol (f.Atom.f a)
| _ -> a ^ ":" ^ symbol (f.Atom.f a)

let atom atom =
  let r = atom.Atom.relation in
  let f = format_map atom in
  let keystr = Lset.mapconcat ", " f (RN.key r) in
  let nonkeystr = Lset.mapconcat ", " f Lset.(r.RN.attributes - RN.key r) in
  sprintf "\\atom{%s}{%s}{%s}" r.RN.name keystr nonkeystr

let tab = "\\hspace*{0.5em}"
let br tabs = "$\\\\" ^ tabs ^ "$"

let rec drcf tabs = function
  | Drc.Certain f -> sprintf "\\certain{%s}" (drcf tabs f)
  | Drc.True -> "\\true"
  | Drc.False -> "\\false"
  | Drc.Atom f -> atom f
  | Drc.And s ->
    let drcf = drcf (tabs ^ tab) in
    sprintf "(%s%s)" (br tabs) (Lset.mapconcat ((br tabs) ^ " \\wedge ") drcf s)
  | Drc.Or s ->
    let drcf = drcf (tabs ^ tab) in
    sprintf "(%s%s)" (br tabs) (Lset.mapconcat ((br tabs) ^ " \\vee ") drcf s)
  | Drc.Neg f -> sprintf "\\neg %s" (drcf tabs f)
  | Drc.Implies (s, t) ->
    sprintf "(%s \\rightarrow %s)" (drcf tabs s) (drcf tabs t)
  | Drc.Equiv (s, t) ->
    sprintf "(%s \\leftrightarrow %s)" (drcf tabs s) (drcf tabs t)
  | Drc.Exists (vs, f) ->
    sprintf "\\exists %s %s" (Lset.concat " \\exists " vs) (drcf tabs f)
  | Drc.Forall (vs, f) ->
    sprintf "\\forall %s %s" (Lset.concat " \\forall " vs) (drcf tabs f)

let drc {Drc.free; Drc.formula} =
  sprintf "$\\{%s \\mid %s\\}$"
    (Lset.mapconcat ", " symbol free) (drcf tab formula)

let drcf q = "$" ^ drcf "" q ^ "$"

let trcvar (v, r) = v ^ "_{" ^ r.RN.name ^ "}"
let trcsymbol = function
  | Trc.Map (v, a) -> trcvar v ^ "." ^ a
  | Trc.Constant c -> c
let trcatom (a, b) = trcsymbol a ^ " = " ^ trcsymbol b

let rec trcf tabs = function
  | Trc.Certain q -> sprintf "\\certain{%s}" (trcf tabs q)
  | Trc.True -> "\\true"
  | Trc.False -> "\\false"
  | Trc.Atom f -> trcatom f
  | Trc.And s ->
    let trcf = trcf (tabs ^ tab) in
    sprintf "(%s%s)" (br tabs) (Lset.mapconcat ((br tabs) ^ " \\wedge ") trcf s)
  | Trc.Or s ->
    let trcf = trcf (tabs ^ tab) in
    sprintf "(%s%s)" (br tabs) (Lset.mapconcat ((br tabs) ^ " \\vee ") trcf s)
  | Trc.Neg f -> sprintf "\\neg %s" (trcf tabs f)
  | Trc.Implies (f, g) ->
    sprintf "(%s \\rightarrow %s)" (trcf tabs f) (trcf tabs g)
  | Trc.Equiv (f, g) ->
    sprintf "(%s \\leftrightarrow %s)" (trcf tabs f) (trcf tabs g)
  | Trc.Exists (vs, f) ->
    sprintf "\\exists %s %s" (Lset.mapconcat " \\exists " trcvar vs) (trcf tabs f)
  | Trc.Forall (vs, f) ->
    sprintf "\\forall %s %s" (Lset.mapconcat " \\forall " trcvar vs) (trcf tabs f)

let trc {Trc.free; Trc.formula} =
  sprintf "$\\{%s \\mid %s\\}$"
    (Lset.mapconcat ", " trcsymbol free) (trcf tab formula)

let trcf q = "$" ^ trcf "" q ^ "$"
