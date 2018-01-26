module C = Conjunctive

type node = {
  q : C.t;
  atom : Atom.t;
  keycl : Symbol.variable Lset.t;
  keycl' : Symbol.variable Lset.t;
  attvars : Symbol.variable Lset.t;
  attatoms : Atom.t Lset.t;
  mutable atoms_after : Atom.t Lset.t option;
}

type t = node Lset.t

let make_node q atom =
  let q' = C.(q - atom) in
  let key = C.atomkey q atom in
  let keycl = FunDep.closure (FunDep.set_from_query q') key in
  let keycl' = FunDep.closure (FunDep.set_from_query q) key in
  let rec aux vars atoms =
    let has_inter f = not Lset.(C.atomvars q f ^ vars = empty) in
    let atoms_in = Lset.filter has_inter atoms in
    if Lset.(atoms_in = empty) then vars
    else
      let atoms_out = Lset.(atoms - atoms_in) in
      let of_interest f = Lset.(C.atomvars q f - keycl) in
      let vars = Lset.(vars + mapmerge of_interest atoms_in) in
      aux vars atoms_out
  in
  let attvars = aux Lset.(C.atomvars q atom - keycl) q'.C.atoms in
  let attacked g = not Lset.(C.atomvars q g ^ attvars = empty) in
  let attatoms = Lset.filter attacked q'.C.atoms in
  { q; atom; keycl; keycl'; attvars; attatoms; atoms_after = None }

let eq node node' = Atom.(node.atom = node'.atom)

let make q = Lset.map ~eq (make_node q) q.Conjunctive.atoms

let unattacked ag =
  let attacked = Lset.mapmerge ~eq:Atom.( = ) (fun n -> n.attatoms) ag in
  Lset.(map ~eq:Atom.( = ) (fun n -> n.atom) ag - attacked)

let rec find ag f = match Lset.pop ag with
| None -> failwith "No such atom"
| Some (n, ag) -> if Atom.(n.atom = f) then n else find ag f

let atoms_after ag n = match n.atoms_after with
| Some s -> s
| None ->
    let rec aux atoms_in atoms_next =
      if Lset.size atoms_next = 0 then atoms_in
      else
        let nodes_next = Lset.map ~eq (find ag) atoms_next in
        let s = Lset.mapmerge ~eq:Atom.( = ) (fun n -> n.attatoms) nodes_next in
        let atoms_in =
          if Lset.(atoms_next = sing ~eq:Atom.( = ) n.atom) && Lset.size atoms_in = 0
          then atoms_in
          else Lset.(atoms_in + atoms_next)
        in
        (*let atoms_in = Lset.(atoms_in + atoms_next) in*)
        aux atoms_in Lset.(s - atoms_in)
    in
    let atoms_after = aux (Lset.empty_eq Atom.( = )) (Lset.sing ~eq:Atom.( = ) n.atom) in
    n.atoms_after <- Some atoms_after;
    atoms_after

let strong ag n f = not (Lset.(C.atomkey n.q f <= n.keycl'))
let transitive ag n f =
  Lset.exists (fun n' -> Lset.mem n'.atom n.attatoms && Lset.mem f n'.attatoms) ag
let cyclic ag n f = Lset.mem n.atom (atoms_after ag (find ag f))

let has_cycle ag = Lset.exists (fun n -> Lset.mem n.atom (atoms_after ag n)) ag
