let from_symbol = function
  | Symbol.Constant c -> c
  | Symbol.Variable v -> "\\mathbf{" ^ v ^ "}"

let namify { Conjunctive.atoms; _ } =
  Array.to_list (
    Array.mapi (fun i a -> (a, Printf.sprintf "F%i" i)) (Array.of_list atoms)
  )

let from_schema { Schema.name; _ } = name

let from_atom (({ Atom.relation; _ } as a), node_name) =
  let k, nk = Atom.key_nonkey_symbols a in
  Printf.sprintf
    "\\node (%s) at (P%s) {$%s(\\underline{%s}%s%s)$};"
    node_name node_name
    (ToString.from_schema relation)
    (String.concat ", " (List.map from_symbol k))
    (if Schema.fullkey relation || Schema.voidkey relation then "" else ", ")
    (String.concat ", " (List.map from_symbol nk))

let from_attack names (attacker, attacked, attack_type) =
  let attacker = List.assoc attacker names
  and attacked = List.assoc attacked names in
  match attack_type with
  | AttackGraph.Strong -> Printf.sprintf "\\draw[-latex,thick,bend right=20] (%s) edge (%s);" attacker attacked
  | AttackGraph.Weak -> Printf.sprintf "\\draw[-latex,bend right=20] (%s) edge (%s);" attacker attacked

let place names =
  let a = Array.of_list names in
  let n = Array.length a in
  let places = Array.mapi (fun i (_, name) -> Printf.sprintf "\\coordinate (P%s) at
  (%i:%fcm);" name (i*360/n) (2. *. sqrt (float n))) a in
  let places = Array.to_list places in
  String.concat "\n" places

let from_attack_graph { AttackGraph.query; AttackGraph.attacks } =
  let names = namify query in
  let placements = place names in
  let nodes = String.concat "\n" (List.map from_atom names) in
  let edges = String.concat "\n" (List.map (from_attack names) attacks) in
  Printf.sprintf "%s\n\n%s\n\n%s" placements nodes edges

let show ag =
  ignore (Sys.command "cat preamble.tex > ag.tex");
  let h = open_out "ag.temp" in
  output_string h ag;
  close_out h;
  ignore (Sys.command "cat ag.temp >> ag.tex");
  ignore (Sys.command "cat epilog.tex >> ag.tex");
  ignore (Sys.command "pdflatex ag.tex");
  ignore (Sys.command "evince ag.pdf")

