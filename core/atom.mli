(** Atoms form queries and relations (as ground atoms). They are represented by
  a relation name and a mapping from attribute names to actual symbols. *)

type t = {
  relation : RelationName.t;
  f : RelationName.attribute -> Symbol.t;
}

val key : t -> Symbol.variable Lset.t
(** Returns the set of variables occuring in the atom's key. *)

val vars : t -> Symbol.variable Lset.t
(** Returns the set of variables occuring in the atom. *)

val substitute : Substitution.t -> t -> t
(** Applies a substitution to the atom. *)

val is_ground : t -> bool
(** An atom is ground if it uses no variable. *)

val ( = ) : t -> t -> bool
(** Are two atoms equal ? *)

