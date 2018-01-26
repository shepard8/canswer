(** Symbols are the atomic elements composing queries. Databases are formed by
  constants. *)

type constant = string
type variable = string

type t =
  | Constant of constant
  | Variable of variable

val vars : t Lset.t -> variable Lset.t
(** [vars l] returns the set of variables used in a set [s] of symbols.

  Note that the _oasis configuration forces the project to be compiled with
  the -safe-string option, which ensures that strings are immutable. Hence the
  [variable Lset.t] is guaranteed to remain duplicates-free. *)

val var : variable -> t
val con : variable -> t
