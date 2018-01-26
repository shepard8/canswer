type t = private {
  atoms : Atom.t Lset.t;
  free : Symbol.t Lset.t
}

val empty : t
val ( @ ) : Symbol.t -> t -> t
val ( + ) : t -> Atom.t -> t
(** [s1 @ ... @ sn @ empty + f1 + ... + fm] returns a new conjunctive in which
  the atoms are [f1 ... fm] and the symbols [s1 ... sn] are free (provided the
  fact that any variable in this sequence occurs in [f1 ... fm]). *)

val ( - ) : t -> Atom.t -> t
(** [q - f] removes the atom [f] from the list of atoms of [q]. *)

val ( -- ) : t -> Atom.t -> t
(** [q -- f] removes the atom [f] from [q], frees any variable in [q] and
  restricts the set of free variables to the set of remaining variables. *)

val vars : t -> Symbol.variable Lset.t
(** Returns the non-free variables of a conjunctive query. *)

val substitute : Substitution.t -> t -> t
(** Applies a substitution to the query (both in the atoms part and on the free
  part). *)

val self_join_free : t -> bool
(** Checks if a given conjunctive query is self-join-free. That is, does not
  use any relation name more than once. *)

val ( = ) : t -> t -> bool
(** Checks that two conjunctive queries are equal (which is stronger than
  equivalent). It checks that the set of free symbols are equal and that the
  sets of atoms are equal. *)

val atomkey : t -> Atom.t -> Symbol.variable Lset.t
(** Returns the set of variables used in an atom key, minus the free variables
  of the query. *)

val atomvars : t -> Atom.t -> Symbol.variable Lset.t
(** Returns the set of variables used in an atom, minus the free variables of
  the query. *)

val merge : t -> t -> t
(** [merge q r] returns a new conjunctive query composed of the atoms of [q]
  and [r] and with any free symbol of [q] or [r] in its free tuple. *)

val split : t -> t Lset.t
(** [split q] splits a conjunctive query in as much as possible conjunctive
  queries sharing no common variable. *)

