open Printf

type t = string

let var (v, r) =
  sprintf "%s AS %s_%s" r.RelationName.name v r.RelationName.name

let sym = function
  | Trc.Map ((v, r), a) -> sprintf "%s_%s.%s" v r.RelationName.name a
  | Trc.Constant c -> sprintf "'%s'" c

let rec sql tabs = function
  | Trc.Certain f -> sprintf "CERTAIN (%s)" (sql tabs f)
  | Trc.True -> "TRUE"
  | Trc.False -> "FALSE"
  | Trc.Atom (s1, s2) -> sprintf "%s = %s" (sym s1) (sym s2)
  | Trc.And s ->
      let sql = sql (tabs ^ "\t") in
      sprintf "(\n%s%s)"
      tabs
      (String.concat ("\n" ^ tabs ^ "AND ") (Lset.map sql s).Lset.l)
  | Trc.Or s ->
      let sql = sql (tabs ^ "\t") in
      sprintf "(\n%s%s)"
      tabs
      (String.concat ("\n" ^ tabs ^ "OR ") (Lset.map sql s).Lset.l)
  | Trc.Neg f -> sprintf "NOT %s" (sql tabs f)
  | Trc.Implies (s, t) -> sql tabs (Trc.dor (Lset.pair t (Trc.dneg s)))
  | Trc.Equiv (s, t) -> sprintf "(%s = %s)" (sql tabs s) (sql tabs t)
  | Trc.Exists (v, f) ->
      let sql = sql (tabs ^ "\t") in
      let froms = String.concat ("\n" ^ tabs ^ "CROSS JOIN ") (Lset.map var v).Lset.l in
      sprintf "EXISTS (\n%sSELECT *\n%sFROM %s\n%sWHERE %s)"
      tabs tabs froms tabs (sql f)
  | Trc.Forall (v, f) -> sql tabs (Trc.dneg (Trc.dexists v (Trc.dneg f)))

let sql = sql "\t"

let from_trc { Trc.free; Trc.formula } =
  let vars = Lset.mapfilter (function Trc.Map (a, _) -> Some (var a) | _ -> None) free in
  let vars = String.concat " CROSS JOIN " vars.Lset.l in
  let vars = if vars = "" then "DUAL" else vars in
  let free = String.concat ", " (Lset.map sym free).Lset.l in
  let free = if free = "" then "TRUE" else free in
  let formula = sql formula in
  Printf.sprintf "SELECT %s FROM %s WHERE %s" free vars formula

let conjunctive q = from_trc (Trc.conjunctive q)
let rewrite ?group ?split q = from_trc (Trc.rewrite ?group ?split q)

