type t

val make : Symbol.variable Lset.t -> Symbol.variable Lset.t -> t
val set_from_query : Conjunctive.t -> t Lset.t

val closure : t Lset.t -> Symbol.variable Lset.t -> Symbol.variable Lset.t

val ( = ) : t -> t -> bool

