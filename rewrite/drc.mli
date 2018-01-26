include Rc.RC with type atom = Atom.t and type variable = Symbol.variable

type t = {
  free : Symbol.t Lset.t;
  formula : formula;
}

val conjunctive : Conjunctive.t -> t
(** Creates a DRC query from a conjunctive query. *)

val rewrite : ?group : bool -> ?split : bool -> Conjunctive.t -> t
(** Rewrites as much as possible a given conjunctive query. In particular, the
  process stops when it encounters a subquery in which no atom is unattacked.
  This function assumes that no variable symbol starts with an underscore. *)

