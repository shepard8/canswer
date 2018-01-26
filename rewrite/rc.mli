module type EQ = sig
  type t
  val ( = ) : t -> t -> bool
end

module type RC = sig
  type atom
  type variable

  type formula = private
    | Certain of formula
    | True
    | False
    | Atom of atom
    | And of formula Lset.t
    | Or of formula Lset.t
    | Neg of formula
    | Implies of formula * formula
    | Equiv of formula * formula
    | Exists of variable Lset.t * formula
    | Forall of variable Lset.t * formula

  (** {3 Inspection} *)

  val ( = ) : formula -> formula -> bool
  (** Checks if two formulas are equal. The order of the [Equiv] elements is not
    taken into account. *)

  val vars : formula -> variable Lset.t
  (** Returns the set of variables used into a given DRC formula. *)

  (** {3 Creation} *)

  val dtrue : formula
  (** The True boolean constant. *)

  val dfalse : formula
  (** The False boolean constant. *)

  val datom : atom -> formula
  (** An atom. *)

  val dcertain : formula -> formula
  (** The answers of the subsequent formula have to be true in each repair of the
    database being queried. As any subsequent [Certain] construction would be
    useless, the return of this function is guaranteed to have no [Certain _]
    subformula. *)

  val dand : formula Lset.t -> formula
  (** [dand s] is the conjunction of formulas in [s]. If [s] contains
    conjunctions, these are merged into the new main conjunction. If [s] contains
    [True], [True] is wiped off of the conjunction. If [s] contains [False], this
    function returns [False]. *)

  val dor : formula Lset.t -> formula
  (** [dor s] is the disjunction of formulas in [s]. If [s] contains
    disjunctions, these are merged into the new main disjunction. If [s] contains
    [True], [True] is returned. If [s] contains [False], it will not appear in
    the final disjunction. *)

  val dneg : formula -> formula
  (** [dneg f] is the negation of [f]. If [f] is a boolean constant, the other
    boolean constant is returned. If [f] is itself a negation (say [f = Neg f']),
    then [f'] is returned. *)

  val dimplies : formula -> formula -> formula
  (** [dimplies f g] returns [f] implies [g]. If [f] or [g] is a boolean
    constant, then the resulting formula is simplified. If [f] and [g] are equal,
    then [True] is returned. *)

  val dequiv : formula -> formula -> formula
  (** [dequiv f g] returns [f] is equivalent to [g]. If [f] or [g] is a boolean
    constant, then the resulting formula is simplified. If [f] and [g] are the
    same formula, then [True] is returned. *)

  val dexists : variable Lset.t -> formula -> formula
  (** [exists v f] quantifies the variables of [v] existencially in [f]. If [v]
    is empty, [f] is returned. If [f] is a boolean constant, the boolean constant
    is returned (that is, it is assumed that the domain of constants is not
    empty). *)

  val dexistsstrict : variable Lset.t -> formula -> formula
  (** same as [dexists] but no assumption is made about the domain. *)

  val dforall : variable Lset.t -> formula -> formula
  (** [forall v f] quantifies the variables of [v] universally in [f]. If [v] is
    empty, [f] is returned. If [f] is a boolean constant, the boolean constant is
    returned (that is, it is assumed that the domain of constants is not empty).
  *)

  val dforallstrict : variable Lset.t -> formula -> formula
  (** same as [dforall] but no assumption is made about the domain. *)

  val map : (formula -> formula option) -> formula -> formula
  (** Applies a function recursively to a formula. When the function returns a
    non-None result when applied to a sub-formula, this sub-formula is replaced
    by the result. *)
end

module Rc : functor (Atom : EQ) -> functor (Variable : EQ) -> (RC with type atom = Atom.t and type variable = Variable.t)
