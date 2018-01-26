(** Substitutions are widely used with conjunctive queries. They are the
  concept underlying homomorphims. If a variable is not in the domain of the
  substitution, it is mapped to itself. Note that the type is not private. *)

type t

val id : t
(** The identity substitution. *)

val domain : t -> Symbol.variable Lset.t
(** Returns the set of variables for which [apply t x <> Symbol.Variable x]. *)

val apply : t -> Symbol.t -> Symbol.t
(** [apply t s] applies the substitution to the symbol [s]. *)

val ( * ) : Symbol.variable -> Symbol.t -> (Symbol.variable * Symbol.t)
val ( + ) : t -> (Symbol.variable * Symbol.t) -> t
(** [t + x * s] returns a new substitution in which variable [x] is mapped to
  [s] and other variables are mapped to [apply t (Symbol.Variable x)].

  Example : [Substitution.(id + "x" * y + "y" * z + "z" * x)] creates a
            "circular" substitution. *)

val is_valuation : t -> bool
(** A substitution is a valuation if every member of its domain is mapped to a
  constant. *)

val ( = ) : t -> t -> bool
(** Checks if two substitutions are equal. *)

