type name = string
type attribute = string

type t = {
  name : string;
  attributes : attribute Lset.t;
  key : attribute -> bool;
}

let make name attrs key =
  { name; attributes = Lset.from_list attrs.Lset.l; key }

let key r = Lset.filter r.key r.attributes

let signature name arity key =
  let attributes = Lset.init (Printf.sprintf "%i") arity in
  let key f = int_of_string f < key in
  make name attributes key

let fullkey name attributes = make name attributes (fun _ -> true)

let binary name a b = make name (Lset.pair a b) (( = ) a)

let ( = ) r s =
  r.name = s.name &&
  Lset.(r.attributes = s.attributes) &&
  Lset.(filter r.key r.attributes = filter s.key s.attributes)

