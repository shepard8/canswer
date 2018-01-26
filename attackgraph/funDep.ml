module C = Conjunctive

type t = {
  premice : Symbol.variable Lset.t;
  conclusion : Symbol.variable Lset.t;
}

let make premice conclusion = { premice; conclusion }

let ( = ) c d =
  Lset.(c.premice = d.premice) &&
  Lset.(c.conclusion - c.premice = d.conclusion - d.premice)

let set_from_query q =
  let from_atom atom = make (C.atomkey q atom) (C.atomvars q atom) in
  Lset.map ~eq:( = ) from_atom q.Conjunctive.atoms

let rec closure s start =
  let fd_in = Lset.(filter (fun fd -> fd.premice <= start) s) in
  let fd_out = Lset.(s - fd_in) in
  if Lset.(fd_in = empty)
  then start
  else closure fd_out Lset.(mapmerge (fun fd -> fd.conclusion) fd_in + start)

