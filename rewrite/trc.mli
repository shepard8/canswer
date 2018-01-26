type symbol =
  | Map of (Symbol.variable * RelationName.t) * RelationName.attribute
  | Constant of Symbol.constant

include Rc.RC
with type variable = Symbol.variable * RelationName.t
and type atom = symbol * symbol

type t = {
  free : symbol Lset.t;
  formula : formula;
}

val conjunctive : Conjunctive.t -> t
(** Creates a DRC query from a conjunctive query. *)

val rewrite : ?group : bool -> ?split : bool -> Conjunctive.t -> t
(** Rewrites as much as possible a given conjunctive query. In particular, the
  process stops when it encounters a subquery in which no atom is unattacked.
  This function assumes that no variable symbol starts with an underscore. *)

