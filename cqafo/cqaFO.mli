type t = private {
  query : Conjunctive.t;
  insourced : Symbol.variable Lset.t;
  outsourced : Symbol.variable Lset.t;
}

val make : Conjunctive.t -> t

val freed_query : t -> Conjunctive.t

val outsource : t -> Symbol.variable -> t

val substitute : t -> Symbol.variable -> Symbol.variable -> t

val ( = ) : t -> t -> bool

