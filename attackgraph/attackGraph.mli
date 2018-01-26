type node = {
  q : Conjunctive.t;
  atom : Atom.t;
  keycl : Symbol.variable Lset.t; (** Closure of key(F) in K(q\{F}) *)
  keycl' : Symbol.variable Lset.t; (** Closure of key(F) in K(q) *)
  attvars : Symbol.variable Lset.t;
  attatoms : Atom.t Lset.t;
  mutable atoms_after : Atom.t Lset.t option;
}

type t = node Lset.t

val make : Conjunctive.t -> t

val has_cycle : t -> bool
val unattacked : t -> Atom.t Lset.t

val strong : t -> node -> Atom.t -> bool
val transitive : t -> node -> Atom.t -> bool
val cyclic : t -> node -> Atom.t -> bool

